{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster
   Copyright   :  (c) 2005-02-03 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   The Postmaster ESMTP Server. See
   <http://postmaster.cryp.to/docs/tutorial.html> for an
   introduction. Note that you should /not/ modify this
   module. In theory, you should be able to achieve any
   effect you like through modifying the contents of
   'Config'. If there is something that appears to be
   impossible to do without modifying the core modules,
   please complain loudly to <mailto:postmaster-dev@lists.cryp.to>.
 -}

module Postmaster where

import Prelude hiding ( catch )
import Foreign
import System.Posix.Signals
import System.IO
import System.IO.Error hiding ( catch, try )
import Network ( listenOn, PortID(..) )
import Network.BSD ( getHostName )
import Network.Socket hiding ( listen, shutdown )
import System.Exit ( ExitCode(..) )
import System.Process
import Control.Exception
import Control.Concurrent
import Control.Monad.RWS hiding ( local )
import Data.List ( isPrefixOf, nub )
import Data.Unique
import qualified Data.Map as FM
import Network.DNS
import MonadEnv
import Rfc2821 hiding ( path )
import BlockIO hiding ( loop )
import Child ( timeout, Timeout )
import Syslog

-- * The @Smtpd@ Monad

type Smtpd a = RWST Config [LogMsg] SmtpdState IO a

-- |@say a b c msg = return ('reply' a b c [msg])@
--
-- The 'SmtpReply' codes 'eventHandler' and 'dataHandler'
-- return determine what Postmaster will do:
--
-- [@1xx@, @2xx@, @3xx@] make the 'sessionState' transition
-- determined determined by 'smtpdFSM'.
--
-- [@4xx@, @5xx@] Do /not/ make the transition.
--
-- [@221@, @421@] Drop the connection after this reply.
--
-- Furthermore, the reply of the 'Greeting' event (the first
-- event triggered when a session starts up) is interpreted
-- as follows: @2xx@ accepts the connection, everything else
-- refuses the connection.

say :: Int -> Int -> Int -> String -> Smtpd SmtpReply
say a b c msg = return (reply a b c [msg])

-- |Convenience wrapper for calling the 'eventHandler' or
-- 'dataHandler'.

trigger :: (Callbacks -> (a -> Smtpd b)) -> a -> Smtpd b
trigger f x = asks (f . callbacks) >>= \f' -> f' x

-- ** Configuration

data Callbacks = CB
  { eventHandler :: Event -> Smtpd SmtpReply
        -- ^ The event handler is called by 'handleDialog'
        -- every time new \"@\\r\\n@\"-terminated input is
        -- available. The event to trigger is determined by
        -- 'smtpdFSM'.
  , dataHandler  :: (Ptr Word8, Int) -> Smtpd ()
        -- ^ Better stay away from this for the time
        -- being. Your data handler of choice is 'feed'.
  , logStream    :: LogMsg -> IO ()
        -- ^ 'Smtpd' is a 'MonadWriter', but at some point
        -- the (lazily!) accumulated log messages have to go
        -- somewhere. Here you can determine where they go
        -- (or won't go). It's probably best to use
        -- 'syslogger'.
  , queryDNS     :: Resolver
        -- ^ Postmaster uses this call-back to access the
        -- domain name service. Initialize with
        -- 'initResolver'.
  }

-- |Use 'mkConfig' to create a 'Config' record with all
-- values initialized to sensible defaults. You may want to
-- change the 'myHeloName', though, which is the result of
-- 'getHostName' per default. That might not be what you
-- want.

data Config = Config
  { callbacks    :: Callbacks
  , ioBufferSize :: Int
        -- ^ This value determines the size of the (static!)
        -- buffers for I\/O. Thus, it also determines the
        -- maximum line length Postmaster will accept. The
        -- default is 1024 bytes.
  , myHeloName   :: String
        -- ^ Initialized with 'getHostName' in 'mkConfig'.
        -- Used by several event handlers; 'SayHelo', for
        -- example.
  , globalEnv    :: MVar Env
        -- ^ A global environment for the entire daemon.
        -- Initialized to 'emptyFM' by 'withConfig'.
  , sendmailPath :: FilePath
        -- ^ Usually: @\/usr\/sbin\/sendmail@. Expect this
        -- entry to be renamed or to change completely in
        -- the future.
  , hIn, hOut    :: Handle
        -- ^ Set by 'smtpdMain'. Stay away from them. Using
        -- the I\/O stream directly will /always/ break
        -- things because of \"pipelining\".
  , sessionID    :: ID
        -- ^ Set by 'smtpdMain'. Uniquely identifies a TCP
        -- session.
  , peerAddr     :: Maybe SockAddr  -- ^ set by 'smtpdMain'
  }
  deriving (Show)

-- |Generated with @'fmap' 'hashUnique' 'newUnique'@.

type ID = Int

-- ** Server State

data SmtpdState = SmtpdState
  { sessionState :: SessionState
        -- ^ Set by 'handleDialog'; do not modify.
  , ioBufferGap  :: Int
        -- ^ I\/O driver internal. Don't touch at all.
  , readTimeout  :: Timeout
        -- ^ The default are 90 seconds. You can change the
        -- initial value in the 'Greeting' event, though. If
        -- a timeout occurs, the session aborts with an
        -- exception (which is caught and logged by
        -- 'runSmtpd').
  , writeTimeout :: Timeout
        -- ^ Like 'readTimeout', but for outbound I\/O.
        -- Timeouts are specified in microseconds (@1\/10^6@
        -- seconds).
  , peerHelo     :: String
        -- ^ Set in 'SayHelo' event. The 'SayHeloAgain',
        -- 'SayEhlo', and 'SayEhloAgain' events are just
        -- wrappers for 'SayHelo' which modify the output
        -- message, so you can safely ignore them.
  , isEhloPeer   :: Bool
        -- ^ Set to 'True' by 'SayEhlo', to 'False' by
        -- 'SayHelo'.
  , mailID       :: ID
        -- ^ Set to a unique value every time 'SetMailFrom'
        -- is triggered.
  , mailFrom     :: Mailbox
        -- ^ Set by 'SetMailFrom'.
  , rcptTo       :: [Target]
        -- ^ Postmaster doesn't set this field at all; this
        -- is your problem. You can probably guess which
        -- event is supposed to do this.
  , localEnv     :: Env
        -- ^ Transient environment for the TCP-session. May
        -- be initialized to your needs in the 'Greeting'
        -- event.
  }
  deriving (Show)

-- |Used by 'smtpdMain' and in the 'ResetState' event, where
-- 'mailFrom', 'rcptTo', and 'mailID' are reset to initial
-- values. The defaults are:
--
-- > initSmtpd = SmtpdState
-- >   { sessionState = Unknown
-- >   , ioBufferGap  = 0
-- >   , readTimeout  = 90*1000000
-- >   , writeTimeout = 90*1000000
-- >   , peerHelo     = ""
-- >   , isEhloPeer   = False
-- >   , mailFrom     = nullPath
-- >   , mailID       = 0
-- >   , rcptTo       = []
-- >   , localEnv     = emptyFM
-- >   }

initSmtpd :: SmtpdState
initSmtpd = SmtpdState
  { sessionState = Unknown
  , ioBufferGap  = 0
  , readTimeout  = 90*1000000
  , writeTimeout = 90*1000000
  , peerHelo     = ""
  , isEhloPeer   = False
  , mailFrom     = nullPath
  , mailID       = 0
  , rcptTo       = []
  , localEnv     = FM.empty
  }

-- ** Mail Targets

data Target = Target [Mailbox] MailHandler MailerStatus
            deriving (Show)

data MailHandler
  = Pipe FilePath [String]
  | Relay
  deriving (Show)

data MailerStatus
  = Ready
  | Live ExternHandle
  | Failed
  | FailedPermanently
  | Succeeded
  deriving (Show)

type ExternHandle = MVar (Handle, Handle, Handle, ProcessHandle)

-- |Create 'Pipe' target and add it to 'rcptTo'. The mailbox
-- parameter is just annotation; but it's a good idea to use
-- it to associate the original e-mail address with the
-- target. If nothing else, it makes the log messages more
-- informative. Note that Postmaster pipes the data section
-- into the program /unmodified/. CRLF line-endings, escaped
-- dot-lines, and all that.

pipe :: [Mailbox] -> FilePath -> [String] -> Smtpd SmtpReply
pipe _   []   _   = fail "Postmaster.pipe: path may not be empty"
pipe rs path args = do
  let target = Target rs (Pipe path args) Ready
  modify (\st -> st { rcptTo = target : (rcptTo st) })
  say 2 5 0 "recipient ok"

-- |Wrapper for 'pipe' which runs the command with
-- @\/bin\/sh -c@ so that you can use in-\/output
-- redirection, piping, etc. This wrapper will also convert
-- the data section to standard text. Meaning that
-- \"@\\r\\n@\" is converted to \"@\\n@\" and
-- \"@\\r\\n..\\r\\n@\" is unquoted to \"@\\r\\n.\\r\\n@\".
-- Procmail needs that, for example.

shell :: [Mailbox] -> String -> Smtpd SmtpReply
shell   _   [] = fail "Postmaster.shell: command may not be empty"
shell mbox cmd = pipe mbox "/bin/sh" [ "-c", cmd']
  where
  cmd'   = toText ++ " | " ++ cmd
  toText = "sed -e 's/\r$//' -e 's/^\\.\\.$/./'"

-- |Create a 'Relay' target and add it to 'rcptTo'.
-- Currently, this just causes execution of 'sendmailPath'.
-- with appropriate flags.

relay :: [Mailbox] -> Smtpd SmtpReply
relay rs = do
  let target = Target rs Relay Ready
  modify (\st -> st { rcptTo = target : (rcptTo st) })
  say 2 5 0 "recipient ok"

-- |Run an external process and store its handle in an
-- 'MVar' with a finalizer attached to it that will close
-- the handles and kill the process when the MVar falls out
-- of scope. The process is run with \"@\/@\" as current
-- directory and an /empty/ enviroment. (This may change.)

extern :: FilePath -> [String] -> IO ExternHandle
extern path args = do
  r <- runInteractiveProcess path args (Just "/") (Just [])
  mv <- newMVar r
  addMVarFinalizer mv (catch (cleanup r) (const (return ())))
  let (hin,_,_,_) = r
  hSetBuffering hin NoBuffering
  return mv
    where
    cleanup (hin, hout, herr, pid) = do
      terminateProcess pid
      hClose hin >> hClose hout >> hClose herr
      safeWaitForProcess pid
      return ()

-- |Wait 30 seconds max. If the process hasn't terminated by
-- then, throw an exception. If the child process has been
-- terminated by a signal, return @ExitFailure 137@. This is
-- a kludge. So it will probably be in here forever.

safeWaitForProcess :: ProcessHandle -> IO ExitCode
safeWaitForProcess pid =
  timeout maxwait loop >>= maybe badluck return
    where
    loop    = catch loop' (\_ -> return (ExitFailure 137))
    loop'   = wait >> getProcessExitCode pid >>= maybe loop' return
    wait    = threadDelay 1000000 -- 1 second
    maxwait = 30000000            -- 30 seconds
    badluck = fail "timeout while waiting for external process"

-- |Make a 'Ready' target 'Live'.

startTarget :: Target -> Smtpd Target

startTarget (Target rs mh@(Pipe path args) Ready) = do
  yell (StartExternal rs (path:args))
  mv <- liftIO (extern path args)
  return (Target rs mh (Live mv))

startTarget (Target rs Relay Ready) = do
  from <- gets mailFrom
  mta <- asks sendmailPath
  let flags = [ "-f" ++ show from ] ++ map show rs
      t'    = Target rs (Pipe mta flags) Ready
  Target _ _ mst <- startTarget t'
  return (Target rs Relay mst)

startTarget t = yell (UnknownStartTarget t) >> return t

-- |Give a target a chunk of data.

feedTarget :: (Ptr Word8, Int) -> Target -> Smtpd Target
feedTarget (ptr,n) t@(Target _ _ (Live mv)) = do
  liftIO (withMVar mv (\(hin,_,_,_) -> hPutBuf hin ptr n))
  return t

feedTarget _ t = yell (UnknownFeedTarget t) >> return t

-- |Close a target.

closeTarget :: Target -> Smtpd Target
closeTarget t@(Target _ _ (Live mv)) = do
  liftIO (withMVar mv (\(hin,_,_,_) -> hClose hin))
  return t

closeTarget t = return t

-- |Update a targets 'MailerStatus' to signify success or
-- failure.

commitTarget :: Target -> Smtpd Target
commitTarget t@(Target rs mh (Live mv)) = do
  rc <- liftIO $ do
    (hin,hout,herr,pid) <- takeMVar mv
    catch (hClose hin) (const (return ()))
    safeWaitForProcess pid
      `finally` hClose hout
      `finally` hClose herr
  yell (ExternalResult t rc)
  if rc == ExitSuccess
     then return (Target rs mh Succeeded)
     else if    (rc == ExitFailure 65)   -- EX_DATAERR
             || (rc == ExitFailure 67)   -- EX_NOUSER
             || (rc == ExitFailure 68)   -- EX_NOHOST
             then return (Target rs mh FailedPermanently)
             else return (Target rs mh Failed)

commitTarget t = yell (UnknownCommitTarget t) >> return t

-- ** Generic Environment

local :: (MonadState SmtpdState m) => EnvT a -> m a
local f = do
  st <- get
  let (a, env') = runState f (localEnv st)
  put st { localEnv = env' }
  return a

global :: EnvT a -> Smtpd a
global f = asks globalEnv >>= global' f

-- ** DNS Resolving

queryA :: HostName -> Smtpd (Maybe [HostAddress])
queryA h = asks (queryDNS . callbacks) >>= \r -> liftIO $ query resolveA r h

queryPTR :: HostAddress -> Smtpd (Maybe [HostName])
queryPTR h = asks (queryDNS . callbacks) >>= \r -> liftIO $ query resolvePTR r h

queryMX :: HostName -> Smtpd (Maybe [(HostName, HostAddress)])
queryMX h = asks (queryDNS . callbacks) >>= \r -> liftIO $ query resolveMX r h

-- ** Logging

data LogMsg = LogMsg ID SmtpdState LogEvent
            deriving (Show)

data LogEvent
  = Msg String
  | StartSession Config
  | EndSession
  | Input String
  | Output String
  | AcceptConn SockAddr
  | DropConn SockAddr
  | UserShutdown
  | ReadTimeout
  | WriteTimeout
  | BufferOverflow
  | CaughtException Exception
  | CaughtIOError IOException
  | StartEventHandler String Event
  | EventHandlerResult String Event SmtpCode
  | CurrentState
  | AssignMailID ID
  | UnknownStartTarget Target
  | UnknownFeedTarget Target
  | UnknownCommitTarget Target
  | StartExternal [Mailbox]  [String]
  | FeedTarget Target String
  | ExternalResult Target ExitCode
  deriving (Show)

-- |Given a log event, construct a 'LogMsg' and write it to
-- the monad's log stream with 'tell'. Every log event
-- contains the current 'SmtpdState' and the 'sessionID'.

yell :: LogEvent -> Smtpd ()
yell e = do
  sid <- asks sessionID
  st <- get
  tell [LogMsg sid st e]

-- |A version of 'yell' which works outside of 'Smtpd'. It
-- writes directly to 'logStream'.

yellIO :: (MonadIO m) => Config -> SmtpdState -> LogEvent -> m ()
yellIO cfg st e = do
  liftIO ((logStream (callbacks cfg)) (LogMsg (sessionID cfg) st e))

-- * Standard Event and Data Handler

feed :: (Ptr Word8, Int) -> Smtpd ()
feed (ptr,n) = do
  ts <- gets rcptTo >>= mapM (feedTarget (ptr,n))
  modify (\st -> st { rcptTo = ts })

event :: Event -> Smtpd SmtpReply

event Greeting = do
  whoami <- asks myHeloName
  say 2 2 0 (showString whoami " Postmaster ESMTP Server")

event Shutdown = do
  whoami <- asks myHeloName
  say 2 2 1 (showString whoami " closing connection")

event NotImplemened =
  say 5 0 2 "command not implemented"

event (Unrecognized _) =
  say 5 0 0 "unrecognized command"

event (SyntaxErrorIn cmd) =
  say 5 0 1 (showString "syntax error in parameters or arguments of " cmd)

event (SayOK) =
  say 2 5 0 "Massive system failure. Just kidding ... ok."

event (NeedHeloFirst) =
  say 5 0 3 "You should say HELO first."

event (NeedMailFromFirst) =
  say 5 0 3 "What MAIL are you talking about?"

event (NeedRcptToFirst) =
  say 5 0 3 "Care to tell me where I should send it to?"

event (ResetState) = do
  modify (\st -> st { mailFrom     = mailFrom initSmtpd
                    , rcptTo       = rcptTo initSmtpd
                    , mailID       = mailID initSmtpd
                    })
  say 2 5 0 "state reset"

event (SeeksHelp []) =
  say 5 0 2 "Why don't you ask about something specific?"

event (SeeksHelp _) =
  say 5 0 4 "I don't implement HELP with parameters."

event (SayHelo peer) = do
  trigger eventHandler ResetState
  modify (\st -> st { peerHelo = peer, isEhloPeer = False })
  whoami <- asks myHeloName
  say 2 5 0 (showString whoami " Postmaster; pleased to meet you.")

event (SayHeloAgain peer) = do
  whoami <- asks myHeloName
  peer' <- gets peerHelo
  r@(Reply c@(Code suc _ _) _) <- trigger eventHandler (SayHelo peer)
  if (peer /= peer' || suc /= Success)
     then return r
     else return (Reply c [msg whoami])
  where
  msg w = showString w " Yeah, yeah. You said that before."

event (SayEhlo peer) = do
  r <- trigger eventHandler (SayHelo peer)
  case r of
    Reply c@(Code Success _ _) (x:_) -> do
      modify (\st -> st { isEhloPeer = True })
      return (Reply c (x:capabilities))
    Reply _ _   -> return r
  where
  capabilities = [ "PIPELINING" ]

event (SayEhloAgain peer) = do
  whoami <- asks myHeloName
  peer' <- gets peerHelo
  r@(Reply c@(Code suc _ _) (_:xs)) <- trigger eventHandler (SayEhlo peer)
  if (peer /= peer' || suc /= Success)
     then return r
     else return (Reply c ((msg whoami):xs))
  where
  msg w = showString w " Sorry, I wasn't listening for a moment."

event (SetMailFrom mbox) = do
  trigger eventHandler ResetState
  mid <- liftIO (fmap hashUnique newUnique)
  yell (AssignMailID mid)
  modify (\st -> st { mailFrom = mbox
                    , mailID   = mid
                    })
  say 2 5 0 (mbox `shows` " ... sender ok")

event (AddRcptTo mbox) =
  say 5 5 3 (mbox `shows` " ... unknown recipient")

event (StartData) = do
  ts <- gets rcptTo
  let isrelay (Target _ Relay Ready) = True
      isrelay _                      = False
      tlocal  = filter (not . isrelay) ts
      batch   = [ rs | Target rs Relay Ready <- ts ]
      relayt  = case nub (concat batch) of
                  [] -> []
                  rs -> [Target rs Relay Ready]
  ts' <- mapM startTarget (tlocal ++ relayt)
  modify (\st -> st { rcptTo = ts' })
  say 3 5 4 "terminate data with <CRLF>.<CRLF>"

event Deliver = do
  ts <- gets rcptTo >>= mapM closeTarget >>= mapM commitTarget
  modify (\st -> st { rcptTo = ts })
  let isSuccess  (Target _ _ Succeeded) = True
      isSuccess  _                      = False
      oneOK = any isSuccess ts
  let isPermFail (Target _ _ FailedPermanently) = True
      isPermFail _                              = False
      allPermFail = all isPermFail ts
  case (oneOK, allPermFail) of
    (True ,   _  ) -> do mid <- gets mailID
                         say 2 5 0 (mid `shows` " message accepted for delivery")
    (False, False) -> say 4 5 1 "requested action aborted: error in processing"
    (  _  , True ) -> say 5 5 4 "transaction failed"

-- * I\/O Driver

-- |Initialize 'syslog' to use the 'MAIL' facility, then
-- 'listenOn' the given port for incoming connections. When
-- a connection comes in, set the new 'Handle' into
-- 'BlockBuffering' mode using 'ioBufferSize' to determine
-- the size of the buffer. Next, generate a unique 'ID' and
-- store it in 'sessionID'. At last, fork off 'runSmtpd' to
-- handle the connection and go back to listening.

smtpdMain :: Config -> PortID -> IO ()
smtpdMain cfg port = do
  installHandler sigPIPE Ignore Nothing
  withSocketsDo $
    withSyslog "postmaster" [PID, PERROR] MAIL $ do
      bracket (listenOn port) (sClose) $ \s -> do
        setSocketOption s ReuseAddr 1
        loop s
  where
  loop ls = do
    (s,sa) <- accept ls
    forkIO (main (s,sa) `finally` sClose s)
    loop ls
  main (s,sa) = do
    setSocketOption s KeepAlive 1
    h <- socketToHandle s ReadWriteMode
    let size = Just (ioBufferSize cfg)
    hSetBuffering h (BlockBuffering size)
    sid <- fmap hashUnique newUnique
    let cfg' = cfg { hIn = h, hOut = h
                   , peerAddr  = Just sa
                   , sessionID = sid
                   }
    bracket_
      (yellIO cfg' initSmtpd (AcceptConn sa))
      (yellIO cfg' initSmtpd (DropConn sa) >> hClose h)
      (runSmtpd cfg')

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

runSmtpd :: Config -> IO ()
runSmtpd cfg = do
  yellIO cfg initSmtpd (StartSession cfg)
  (r, st) <- runStateT (withConfig cfg (trigger eventHandler Greeting)) initSmtpd
  let logM = yellIO cfg st
      to   = writeTimeout st
      hout = hOut cfg
      safeIO f = timeout to f >>= maybe (fail "write timeout") return
  logM (Output (show r))
  safeIO (hPutStr hout (show r) >> hFlush hout)
  let Reply (Code rc _ _) _ = r
  if rc /= Success then return () else do
    let hin  = hIn cfg
        size = ioBufferSize cfg
        main = smtpdHandler cfg
    catch
      (runLoopNB readTimeout hin size main st >> return ())
      (\e -> case e of
         IOException ie ->
           if isTimeout ie then logM ReadTimeout else
           if isBufferOverflow ie then logM BufferOverflow else
           if not (isUserError ie) then logM (CaughtException e) else
           if ioeGetErrorString ie == "shutdown" then logM UserShutdown else
           if ioeGetErrorString ie == "write timeout" then logM WriteTimeout
           else logM (CaughtIOError ie)
         _  -> logM (CaughtException e))

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

-- |Run 'handleData' if the session is in 'HaveData' state,
-- run 'handleDialog' otherwise. Recurse until the entire
-- buffer has been processed. The replies returned by the
-- handers are written to 'hOut'. The stream is flushed when
-- the recursion ends, so for optimal performance the
-- 'Handle' should be in 'BlockBuffering' mode. If the
-- 'eventHandler' (or the 'dataHandler') returns 221 or 421,
-- drop the connection after writing the reply.

smtpdHandler :: Config -> LoopHandler SmtpdState
smtpdHandler cfg (ptr,n) = do
  let logM m   = get >>= \st -> yellIO cfg st m
      safeIO f = gets writeTimeout >>= \to ->
                 liftIO (timeout to f) >>=
                 maybe (fail "write timeout") return
      hout     = hOut cfg
      shutdown = safeIO (hFlush hout >> fail "shutdown")
  sst <- gets sessionState
  r' <- if sst == HaveData
           then withConfig cfg (handleData (ptr,n))
           else withConfig cfg (handleDialog (ptr,n))
  case r' of
    Just (r,i) -> do
      logM (Output (show r))
      safeIO (hPutStr hout (show r))
      let term (Reply (Code Success Connection 1) _)          = True
          term (Reply (Code TransientFailure Connection 1) _) = True
          term _                                              = False
      when (term r) shutdown
      modify (\st -> st { ioBufferGap = (ioBufferGap st) + i })
      let ptr' = ptr `plusPtr` i
          n'   = assert (i <= n) (n - i)
      smtpdHandler cfg (ptr',n')
    Nothing    -> do
      gap <- gets ioBufferGap
      if gap == 0
         then return Nothing
         else do safeIO (hFlush hout)
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

-- * Helpers

strstr :: [Word8] -> [Word8] -> Maybe Int
strstr tok = find 0
  where
  find  _      []         = Nothing
  find pos ls@(_:xs)
    | tok `isPrefixOf` ls = Just (pos + length tok)
    | otherwise           = find (pos + 1) xs

instance Show Callbacks where
  show _ = "<Postmaster.Callbacks>"

instance Show (MVar Env) where
  show _ = "<globalEnv>"

instance Show ExternHandle where
  show _ = "<ExternHandle>"

mkConfig :: (Config -> IO a) -> IO a
mkConfig f =
  initResolver [NoErrPrint,NoServerWarn] $ \resolver -> do
  theEnv <- newMVar emptyEnv
  whoami <- getHostName
  let cbs = CB { eventHandler = event
               , dataHandler  = feed
               , logStream    = syslogger
               , queryDNS     = resolver
               }
      cfg = Config
              { callbacks    = cbs
              , hIn          = stdin
              , hOut         = stdout
              , ioBufferSize = 1024
              , peerAddr     = Nothing
              , myHeloName   = whoami
              , sessionID    = 0
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
