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
import Control.Exception
import Control.Concurrent.MVar
import Control.Monad.RWS hiding ( local )
import System.IO
import System.IO.Error hiding ( catch )
import System.Posix.Signals
import Network ( listenOn, PortID(..) )
import Network.Socket hiding ( shutdown )
import Network.DNS
import Foreign
import Postmaster.Base
import Postmaster.Event
import Rfc2821
import Syslog
import BlockIO hiding ( loop )
import Child
import MonadEnv

-- |An @SmtpdHandler@ is a 'Handler' as defined in
-- "BlockIO", except for that it is a 'RWST' monad instead
-- of a 'StateT' monad.

type SmtpdHandler a = (Ptr Word8, Int) -> Smtpd (Maybe (a, Int))

-- |Given a configuration, demote an 'Smtpd' computation to
-- an ordinary 'StateT' monad.

withConfig :: Config -> Smtpd a -> StateT SmtpdState IO a
withConfig cfg f = do
  st <- get
  (a, st', w) <- liftIO (runRWST f cfg st)
  put st'
  liftIO (mapM_ (logStream (callbacks cfg)) w)
  return a

-- |Initialize 'syslog' to use the 'MAIL' facility, then
-- 'listenOn' the given port for incoming connections. When
-- a connection comes in, set the new 'Handle' into
-- 'BlockBuffering' mode using 'ioBufferSize' to determine
-- the size of the buffer. Next, generate a unique 'ID' and
-- store it in 'sessionID'. At last, fork off 'runSmtpd' to
-- handle the connection and go back to listening.

smtpdMain :: (Config -> Config) -> PortID -> IO ()
smtpdMain f port = do
  installHandler sigPIPE Ignore Nothing
  withSocketsDo $
    withSyslog "postmaster" [PID, PERROR] MAIL $ do
      mkConfig $ \cfg ->
        bracket (listenOn port) (sClose) $ \s -> do
          setSocketOption s ReuseAddr 1
          loop (f cfg) s
  where
  loop cfg ls = do
    (s,sa) <- accept ls
    spawn (main cfg (s,sa) `finally` sClose s)
    loop cfg ls
  main cfg (s,sa) = do
    setSocketOption s KeepAlive 1
    h <- socketToHandle s ReadWriteMode
    let size = Just (ioBufferSize cfg)
    hSetBuffering h (BlockBuffering size)
    let cfg' = cfg { peerAddr  = Just sa }
    bracket_
      (yellIO cfg' initSmtpd (AcceptConn sa))
      (yellIO cfg' initSmtpd (DropConn sa) >> hClose h)
      (runSmtpd h h cfg')

-- |Run a SMTP server with the given configuration. The
-- function will log 'StartSession', then trigger
-- 'Greeting'. If the greeting call-back returns a
-- non-success reply (not 2xx), refuse the session.
-- Exceptions thrown by the server will be caught and
-- logged, then the session aborts. Otherwise run
-- 'smtpdHandler' with 'runLoopNB'. You have to set /all/
-- fields of 'Config' when using @runSmtpd@ directly because
-- 'smtpdMain', which is by-passed, doesn't do it for you in
-- this case.

runSmtpd :: Handle -> Handle -> Config -> IO ()
runSmtpd hIn hOut cfg = do
  (r, st) <- runStateT (withConfig cfg (trigger eventHandler Greeting)) initSmtpd
  yellIO cfg initSmtpd (StartSession cfg)
  let logM = yellIO cfg st
      to   = writeTimeout st
  let safeIO f = timeout to f >>= maybe (fail "write timeout") return
  logM (Output (show r))
  safeIO (hPutStr hOut (show r) >> hFlush hOut)
  let Reply (Code rc _ _) _ = r
  if rc /= Success then return () else do
    let size = ioBufferSize cfg
        main = smtpdHandler hOut cfg
    catch
      (runLoopNB readTimeout hIn size main st >> return ())
      (\e -> case e of
         IOException ie ->
           if isTimeout ie then logM ReadTimeout else
           if isBufferOverflow ie then logM BufferOverflow else
           if not (isUserError ie) then logM (CaughtException e) else
           if ioeGetErrorString ie == "shutdown" then logM UserShutdown else
           if ioeGetErrorString ie == "write timeout" then logM WriteTimeout
           else logM (CaughtIOError ie)
         _  -> logM (CaughtException e))


-- |Run 'handleData' if the session is in 'HaveData' state,
-- run 'handleDialog' otherwise. Recurse until the entire
-- buffer has been processed. The replies returned by the
-- handers are written to 'hOut'. The stream is flushed when
-- the recursion ends, so for optimal performance the
-- 'Handle' should be in 'BlockBuffering' mode. If the
-- 'eventHandler' (or the 'dataHandler') returns 221 or 421,
-- drop the connection after writing the reply.

smtpdHandler :: Handle -> Config -> LoopHandler SmtpdState
smtpdHandler hOut cfg (ptr,n) = do
  let logM m   = get >>= \st -> yellIO cfg st m
      safeIO f = gets writeTimeout >>= \to ->
                 liftIO (timeout to f) >>=
                 maybe (fail "write timeout") return
      shutdown = safeIO (hFlush hOut >> fail "shutdown")
  sst <- gets sessionState
  r' <- if sst == HaveData
           then withConfig cfg (handleData (ptr,n))
           else withConfig cfg (handleDialog (ptr,n))
  case r' of
    Just (r,i) -> do
      logM (Output (show r))
      safeIO (hPutStr hOut (show r))
      let term (Reply (Code Success Connection 1) _)          = True
          term (Reply (Code TransientFailure Connection 1) _) = True
          term _                                              = False
      when (term r) shutdown
      modify (\st -> st { ioBufferGap = (ioBufferGap st) + i })
      let ptr' = ptr `plusPtr` i
          n'   = assert (i <= n) (n - i)
      smtpdHandler hOut cfg (ptr',n')
    Nothing    -> do
      gap <- gets ioBufferGap
      if gap == 0
         then return Nothing
         else do safeIO (hFlush hOut)
                 modify (\st -> st { ioBufferGap = 0 })
                 return (Just ((), gap))

-- |Use 'dataHandler' callback to process the input. If the
-- buffer contains @\<CRLF\>.\<CRLF\>@, trigger a 'Deliver'
-- event, then a 'ResetState' event, then return the result
-- of the delivery.

handleData :: SmtpdHandler SmtpReply
handleData (ptr,n) = do
  buf <- liftIO (peekArray n ptr)
  let eod = map (toEnum . fromEnum) "\r\n.\r\n"
  case strstr eod buf of
    Nothing -> do let n' = max 0 (n - 4)
                  when (n' > 0) (trigger dataHandler (ptr, n'))
                  gap <- gets ioBufferGap
                  modify (\st -> st { ioBufferGap = gap + n' })
                  return Nothing
    Just i  -> do trigger dataHandler (ptr, (i-3))
                  r <- trigger eventHandler Deliver
                  trigger eventHandler ResetState
                  modify (\st -> st { sessionState = HaveHelo })
                  return (Just (r, i))

-- |If there is one, consume the first line from the buffer
-- and run it through 'smtpdFSM'. Then trigger the 'Event'
-- the machine returns and make the 'sessionState'
-- transition accordingly if the event handler returns
-- \"success\" (meaning: 1xx, 2xx, 3xx).

handleDialog :: SmtpdHandler SmtpReply
handleDialog (ptr,n) = do
  buf <- liftIO (peekArray n ptr)
  let crlf = map (toEnum . fromEnum) "\r\n"
  case strstr crlf buf of
    Nothing -> return Nothing
    Just i  -> do
      sst <- gets sessionState
      let line     = map (toEnum . fromEnum) (take i buf)
          (e,sst') = runState (smtpdFSM line) sst
      yell (Input line)
      r <- trigger eventHandler e
      modify $ \st -> case r of
        Reply (Code Unused0 _ _) _          -> error "Unused?"
        Reply (Code TransientFailure _ _) _ -> st
        Reply (Code PermanentFailure _ _) _ -> st
        _                                   -> st { sessionState = sst' }
      return (Just (r, i))

mkConfig :: (Config -> IO a) -> IO a
mkConfig f =
  initResolver [NoErrPrint,NoServerWarn] $ \resolver -> do
  theEnv <- newMVar emptyEnv
  let cbs = CB { eventHandler = mkEvent "peti.cryp.to"
               , dataHandler  = feed
               , logStream    = syslogger
               , queryDNS     = resolver
               }
      cfg = Config
              { callbacks    = cbs
              , ioBufferSize = 1024
              , peerAddr     = Nothing
              , globalEnv    = theEnv
              , sendmailPath = "/usr/sbin/sendmail"
              }
  f cfg

-- |Currently logs all messages with 'syslog' and priority
-- 'Info'. Will be polished when I have nothing better to
-- do.

syslogger :: LogMsg -> IO ()
syslogger (LogMsg sid _ e) = do
  let msg = (showString "SID ") . (sid `shows`) . (':':)
            . (' ':) . (shows e) $ ""
  syslog Info msg

-- |Use this call-back to disable DNS resolving. It will
-- always return @('Answer' 'sSYSTEMFAIL' Nothing (Just host)
-- (-1) [])@.

dummyDNS :: Resolver
dummyDNS host _ _ = newMVar
  (Answer sSYSTEMFAIL Nothing (Just host) (-1) [])

-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-ghci-program-args: ( "-ladns" "-lcrypto" ) ***
-- End: ***
