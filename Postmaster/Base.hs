{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.Base
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

module Postmaster.Base where

import Prelude hiding ( catch )
import Foreign
import System.IO
import Network.Socket hiding ( listen, shutdown )
import System.Exit ( ExitCode(..) )
import System.Process
import Control.Exception
import Control.Concurrent
import Control.Monad.RWS hiding ( local )
import qualified Data.Map as FM
import Network.DNS
import MonadEnv
import Rfc2821 hiding ( path )
import Child ( timeout, Timeout )

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

-- * Standard Event and Data Handler

feed :: (Ptr Word8, Int) -> Smtpd ()
feed (ptr,n) = do
  ts <- gets rcptTo >>= mapM (feedTarget (ptr,n))
  modify (\st -> st { rcptTo = ts })

-- * Helpers

instance Show Callbacks where
  show _ = "<Postmaster.Callbacks>"

instance Show (MVar Env) where
  show _ = "<globalEnv>"

instance Show ExternHandle where
  show _ = "<ExternHandle>"



-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-ghci-program-args: ( "-ladns" "-lcrypto" ) ***
-- End: ***
