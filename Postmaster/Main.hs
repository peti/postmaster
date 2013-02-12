{- |
   Module      :  Postmaster.Main
   Copyright   :  (c) 2004-2008 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   Postmaster's IO driver and general initialization functions.
-}

module Postmaster.Main where

import Prelude hiding ( catch )
import Data.Maybe
import Control.Concurrent.MVar
import Control.Monad.RWS hiding ( local )
import Control.Monad.State
import System.IO
import System.Posix.Signals
import Network ( PortID(..) )
import Network.Socket
import Network.BSD ( getHostName )
import ADNS
import Foreign
import Postmaster.Base
import Postmaster.FSM
import Postmaster.FSM.DNSResolver  ( setDNSResolver  )
import Postmaster.FSM.EventHandler ( setEventHandler )
import Postmaster.FSM.PeerAddr     ( setPeerAddr     )
import Postmaster.FSM.SessionState ( setSessionState )
import Postmaster.IO
import Text.ParserCombinators.Parsec.Rfc2821
import System.Posix.Syslog

-- * Speaking ESMTP

-- |This function ties it all together to build a
-- 'BlockHandler' for "System.IO.Driver".

smtpdHandler :: WriteHandle -> GlobalEnv -> BlockHandler SmtpdState
smtpdHandler hOut theEnv buf = runSmtpd (smtpd buf >>= handler) theEnv
  where
  handler :: ([SmtpReply], Buffer) -> Smtpd Buffer
  handler (rs, buf') = do
    safeWrite (hPutStr hOut (concatMap show rs) >> hFlush hOut)
    when (any isShutdown rs) (fail "shutdown")
    return buf'

-- |The unified interface to dialog and data section. This
-- function relies on the fact that pipelining ends with
-- @DATA@ commands: dialog and payload must /not/ come in a
-- single buffer. See
-- <http://www.faqs.org/rfcs/rfc2920.html> section 3.1.

smtpd :: Buffer -> Smtpd ([SmtpReply], Buffer)
smtpd buf@(Buf _  _  0) = return ([], buf)
smtpd buf@(Buf _ ptr n) = do
  sst <- getSessionState
  if sst == HaveData
      then do (r, buf') <- feed buf
              return (maybeToList r, buf')
      else do xs <- liftIO (peekArray (fromIntegral n) ptr)
              let xs'  = map (toEnum . fromEnum) xs
                  ls'  = splitList "\r\n" xs'
                  ls   = reverse . tail $ reverse ls'
                  rest = last ls'
                  i    = length xs - length rest
              rs <- mapM handleDialog ls
              buf' <- liftIO $ flush (fromIntegral i) buf
              return (rs, buf')

handleDialog :: String -> Smtpd SmtpReply
handleDialog line = do
  sst <- getSessionState
  let (e, sst') = runState (smtpdFSM (fixCRLF line)) sst
  r <- trigger e
  case (e, isSuccess r) of
    (_, True)          -> setSessionState sst'
    (StartData, False) -> trigger ResetState >> setSessionState HaveHelo
    _                  -> return ()
  return r


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

-- |Run the given computation with an initialized global
-- environment for 'Smtpd'. The environment is destroyed
-- when this function returns.

withGlobalEnv :: HostName           -- ^ 'myHeloName'
              -> Resolver
              -> EventT
              -> (GlobalEnv -> IO a)
              -> IO a
withGlobalEnv myHelo dns eventT f = do
  let eventH  = eventT (mkEvent myHelo "/tmp/test-spool")
      initEnv = do setDNSResolver dns
                   setEventHandler eventH
  newMVar (execState initEnv emptyEnv) >>= f


-- * ESMTP Network Daemon

smtpdServer :: Capacity -> GlobalEnv -> SocketHandler
smtpdServer cap theEnv =
  handleLazy ReadWriteMode $ \(h, Just sa) -> do
    hSetBuffering h (BlockBuffering (Just (fromIntegral cap)))
    let st = execState (setPeerAddr sa) emptyEnv
    smtpdMain cap theEnv h h st >> return ()

smtpdMain :: Capacity
          -> GlobalEnv
          -> ReadHandle
          -> WriteHandle
          -> SmtpdState
          -> IO SmtpdState
smtpdMain cap theEnv hIn hOut initST = do
  let greet = do r <- trigger Greeting
                 safeReply hOut r >> safeFlush hOut
                 to <- getReadTimeout
                 sid <- mySessionID
                 return (r, to, sid)
  ((r, to, sid), st) <- runSmtpd greet theEnv initST
  st' <- case r of
    Reply (Code Success _ _) _  -> do
      let getTO  = evalState (getVarDef (mkVar "ReadTimeout") to)
          yellIO = syslogger . LogMsg sid st
          hMain  = smtpdHandler hOut theEnv
          errH e st' = yellIO (CaughtException e) >> return st'
      runLoopNB getTO errH hIn cap hMain st
    _ -> return st
  let bye = do sst <- getSessionState
               when (sst /= HaveQuit)
                 (trigger Shutdown >> return ())
  fmap snd (runSmtpd bye theEnv st')

main' :: Capacity -> PortID -> EventT -> IO ()
main' cap port eventT = do
  _ <- installHandler sigPIPE Ignore Nothing
  _ <- installHandler sigCHLD (Catch (return ())) Nothing
  whoami <- getHostName
  withSocketsDo $
    withSyslog "postmaster" [PID, PERROR] MAIL $
      initResolver [NoErrPrint,NoServerWarn] $ \dns ->
        withGlobalEnv whoami dns eventT $
          listener port . smtpdServer cap

main :: IO ()
main = main' 1024 (PortNumber 2525) id

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
