{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.Main
   Copyright   :  (c) 2005-02-05 by Peter Simons
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
import Network ( listenOn, PortID(..) )
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
import Child
import MonadEnv

-- * Running 'Smtpd' Computations

-- |Run the given 'Smtpd' computation and write the logging
-- output via the given 'Logger'. Use 'withGlobalEnv' to
-- create the global environment and 'emptyEnv' to
-- initialize the 'SmtpdState'.

runSmtpd' :: Logger -> Smtpd a -> GlobalEnv -> SmtpdState -> IO (a, SmtpdState)
runSmtpd' logger f = runStateT . withLogger logger f

-- |Convenience wrapper for 'runSmtpd'' with 'Logger'
-- hard-coded to 'syslogger'.

runSmtpd :: Smtpd a -> GlobalEnv -> SmtpdState -> IO (a, SmtpdState)
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
      initEnv = do setval "DNSResolver" (DNSR dns)
                   setval "EventHandler" (EH eventH)
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


-- * The ESMTP Server

smtpd :: WriteHandle -> Buffer -> Smtpd Buffer
smtpd hOut buf@(Buf _ ptr n) = do
  sst <- getSessionState
  (rs, buf') <- if (sst == HaveData)
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
  safeWrite (hPutStr hOut (concatMap show rs) >> hFlush hOut)
  let term (Reply (Code Success Connection 1) _)          = True
      term (Reply (Code TransientFailure Connection 1) _) = True
      term _                                              = False
  when (any term rs) (fail "shutdown")
  return buf'

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

smtpdHandler :: WriteHandle -> GlobalEnv -> BlockHandler SmtpdState
smtpdHandler hOut theEnv buf = runSmtpd (smtpd hOut buf) theEnv

smtpdMain :: Capacity -> ReadHandle -> WriteHandle -> GlobalEnv -> IO ()
smtpdMain cap hIn hOut theEnv = do
  let greet = trigger Greeting >>= \r -> safeReply hOut r >> return r
  (r, st) <- runSmtpd greet theEnv emptyEnv
  let Reply (Code rc _ _) _ = r
  undefined
  when (rc == Success) $ do
    catch
      (runLoop hIn cap (smtpdHandler hOut theEnv) st >> return ())
      (\e -> case e of
         IOException _ -> return ()
         _             -> return ())

main' :: Capacity -> EventT -> PortID -> IO ()
main' cap eventT port = do
  installHandler sigPIPE Ignore Nothing
  installHandler sigCHLD (Catch (return ())) Nothing
  whoami <- getHostName
  withSocketsDo $
    withSyslog "postmaster" [PID, PERROR] MAIL $
      initResolver [NoErrPrint,NoServerWarn] $ \dns ->
        withGlobalEnv whoami dns eventT $ \theEnv ->
        bracket (listenOn port) (sClose) $ \s -> do
          setSocketOption s ReuseAddr 1
          loop theEnv s
  where
  loop theEnv ls = do
    (s,sa) <- accept ls
    spawn (server theEnv (s,sa) `finally` sClose s)
    loop theEnv ls
  server theEnv (s,sa) = do
    setSocketOption s KeepAlive 1
    h <- socketToHandle s ReadWriteMode
    hSetBuffering h (BlockBuffering (Just (fromIntegral cap)))
    smtpdMain cap h h theEnv `finally` hClose h

main :: IO ()
main = main' 1024 id (PortNumber 2525)

-- -- mkConfig :: (Config -> IO a) -> IO a
-- -- mkConfig f =
-- --   initResolver [NoErrPrint,NoServerWarn] $ \resolver -> do
-- --   theEnv <- newMVar emptyEnv
-- --   let cbs = CB { eventHandler = mkEvent "peti.cryp.to"
-- --                , dataHandler  = feed
-- --                , logStream    = syslogger
-- --                , queryDNS     = resolver
-- --                }
-- --       cfg = Config
-- --               { callbacks    = cbs
-- --               , peerAddr     = Nothing
-- --               , globalEnv    = theEnv
-- --               , sendmailPath = "/usr/sbin/sendmail"
-- --               }
-- --   f cfg

-- |Currently logs all messages with 'syslog' and priority
-- 'Info'. Will be polished when I have nothing better to
-- do.



-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-ghci-program-args: ( "-ladns" "-lcrypto" ) ***
-- End: ***
