{- |
   Module      :  Postmaster.Main
   Copyright   :  (c) 2005-02-06 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   Postmaster's IO driver for "BlockIO" and general
   initialization functions.
 -}

module Postmaster.Main where

import Prelude hiding ( catch )
import Data.Maybe
import Control.Exception
import Control.Concurrent.MVar
import Control.Monad.RWS hiding ( local )
import System.IO
import System.Posix.Signals
import Network ( PortID(..) )
import Network.Socket
import Network.BSD ( getHostName )
import Network.DNS
import Foreign
import Postmaster.Base
import Postmaster.Event
import Postmaster.IO
import Rfc2821
import Syslog
import BlockIO
import MonadEnv

-- * Speaking ESMTP

smtpdHandler :: WriteHandle -> GlobalEnv -> BlockHandler SmtpdState
smtpdHandler hOut theEnv buf = runSmtpd (smtpd buf >>= handler) theEnv
  where
  handler :: ([SmtpReply], Buffer) -> Smtpd Buffer
  handler (rs, buf') = do
    safeWrite (hPutStr hOut (concatMap show rs) >> hFlush hOut)
    let term (Reply (Code Success Connection 1) _)          = True
        term (Reply (Code TransientFailure Connection 1) _) = True
        term _                                              = False
    when (any term rs) (fail "shutdown")
    return buf'


-- |This function relies on the fact that pipelining ends
-- with @DATA@ commands: dialog and payload must /not/ come
-- in a single buffer. See
-- <http://www.faqs.org/rfcs/rfc2920.html> section 3.1.

smtpd :: Buffer -> Smtpd ([SmtpReply], Buffer)
smtpd buf@(Buf _ ptr n) = do
  sst <- getSessionState
  if (sst == HaveData)
      then do (r, buf') <- handleData buf
              return (maybeToList r, buf')
      else do xs <- liftIO (peekArray (fromIntegral n) ptr)
              let xs'  = map (toEnum . fromEnum) xs
                  ls'  = splitList "\r\n" xs'
                  ls   = reverse . tail $ reverse ls'
                  rest = head $ reverse ls'
                  i    = length xs - length rest
              rs <- mapM handleDialog ls
              buf' <- liftIO $ flush (fromIntegral i) buf
              return (rs, buf')

handleData :: Buffer -> Smtpd (Maybe SmtpReply, Buffer)
handleData buf@(Buf _ ptr n) = do
  xs <- liftIO (peekArray (fromIntegral n) ptr)
  let eod = map (toEnum . fromEnum) "\r\n.\r\n"
  case strstr eod xs of
    Nothing -> do let n' = max 0 (n - 4)
                  feed (ptr, fromIntegral n')
                  buf' <- liftIO $ flush n' buf
                  return (Nothing, buf')
    Just i  -> do feed (ptr, (i-3))
                  r <- trigger Deliver
                  trigger ResetState
                  setSessionState HaveHelo
                  buf' <- liftIO $ flush (fromIntegral i) buf
                  return (Just r, buf')

handleDialog :: String -> Smtpd SmtpReply
handleDialog line' = do
  let line = fixCRLF line'
  sst <- getSessionState
  let (e,sst') = runState (smtpdFSM line) sst
  r <- trigger e
  case r of
    Reply (Code Unused0 _ _) _          -> fail "Unused?"
    Reply (Code TransientFailure _ _) _ -> return ()
    Reply (Code PermanentFailure _ _) _ -> return ()
    _                                   -> setSessionState sst'
  return r


-- * ESMTP Network Daemon

smtpdServer :: Capacity -> GlobalEnv -> SocketHandler
smtpdServer cap theEnv =
  handleLazy ReadWriteMode $ \(h, Just sa) -> do
    hSetBuffering h (BlockBuffering (Just (fromIntegral cap)))
    let st = execState (setval (mkVar "PeerAddr") sa) emptyEnv
    smtpdMain cap theEnv h h st

smtpdMain :: Capacity
          -> GlobalEnv
          -> ReadHandle
          -> WriteHandle
          -> SmtpdState
          -> IO ()
smtpdMain cap theEnv hIn hOut initST = do
  let greet = do r <- trigger Greeting
                 safeReply hOut r >> safeFlush hOut
                 to <- getReadTimeout
                 sid <- mySessionID
                 return (r, to, sid)
  ((r, to, sid), st) <- runSmtpd greet theEnv initST
  let Reply (Code rc _ _) _ = r
  when (rc == Success) $ do
    let getTO  = evalState (getDefault (mkVar "ReadTimeout") to)
        yellIO = syslogger . LogMsg sid st
        hMain  = smtpdHandler hOut theEnv
    catch
      (runLoopNB getTO hIn cap hMain st >> return ())
      (\e -> case e of
         IOException ie -> yellIO (CaughtIOError ie)
         _              -> yellIO (CaughtException e))

main' :: Capacity -> PortID -> EventT -> IO ()
main' cap port eventT = do
  installHandler sigPIPE Ignore Nothing
  installHandler sigCHLD (Catch (return ())) Nothing
  whoami <- getHostName
  withSocketsDo $
    withSyslog "postmaster" [PID, PERROR] MAIL $
      initResolver [NoErrPrint,NoServerWarn] $ \dns ->
        withGlobalEnv whoami dns eventT $
          listener port . smtpdServer cap

main :: IO ()
main = main' 1024 (PortNumber 2525) id


-- * Running 'Smtpd' Computations

-- |Run the given 'Smtpd' computation and write the logging
-- output via the given 'Logger'. Use 'withGlobalEnv' to
-- create the global environment and 'emptyEnv' to
-- initialize the 'SmtpdState'.

runSmtpd' :: Logger -> Smtpd a -> GlobalEnv -> SmtpdState
          -> IO (a, SmtpdState)
runSmtpd' logger f = runStateT . withLogger logger f

-- |Convenience wrapper for 'runSmtpd'' with 'Logger'
-- hard-coded to 'syslogger'.

runSmtpd :: Smtpd a -> GlobalEnv -> SmtpdState
         -> IO (a, SmtpdState)
runSmtpd = runSmtpd' syslogger

-- ** Initialize Global Environment

-- |Run the given computation with an initialized global
-- environment. The environment is destroyed when this
-- function returns.

withGlobalEnv :: HostName           -- ^ 'myHeloName'
              -> Resolver           -- ^ 'getDNSResolver'
              -> EventT             -- ^ 'getEventHandler'
              -> (GlobalEnv -> IO a)
              -> IO a
withGlobalEnv myHelo dns eventT f = do
  let eventH  = eventT (mkEvent myHelo)
      initEnv = do setval (mkVar "DNSResolver") (DNSR dns)
                   setval (mkVar "EventHandler") (EH eventH)
  newMVar (execState initEnv emptyEnv) >>= f

-- ** Logging

type Logger = LogMsg -> IO ()

-- |Bind the logging back-end to demote 'Smtpd' to an
-- ordinary 'StateT' computation.

withLogger :: Logger -> Smtpd a -> GlobalEnv -> StateT SmtpdState IO a
withLogger logger f theEnv = do
  (a, st, w) <- get >>= liftIO . runRWST f theEnv
  liftIO $ mapM_ logger w
  put st
  return a

-- |Our default logging back-end.

syslogger :: Logger
syslogger (LogMsg sid _ e) = syslog Info $
  showString "SID " . shows sid . (':':) . (' ':) $ show e


-- * Local Variable: @PeerAddr@

getPeerAddr :: Smtpd (Maybe SockAddr)
getPeerAddr = local $ getval (mkVar "PeerAddr")
