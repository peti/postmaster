{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.Base
   Copyright   :  (c) 2005-02-05 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.Base where

import Prelude hiding ( catch )
import Foreign
import System.IO
import Network.Socket hiding ( listen, shutdown )
import System.Exit ( ExitCode(..) )
import Control.Exception
import Control.Concurrent
import Control.Monad.RWS hiding ( local )
import qualified Data.Map as FM
import Network.DNS
import MonadEnv
import Data.Typeable
import Rfc2821 hiding ( path )
import Child
import Postmaster.Extern

-- * The @Smtpd@ Monad

type Smtpd a = RWST Config [LogMsg] SmtpdState IO a

-- |@say a b c msg = return ('reply' a b c [msg])@
--
-- The 'SmtpReply' codes 'eventHandler' and 'dataHandler'
-- return determine what Postmaster will do:
--
-- [@1xx@, @2xx@, @3xx@] make the 'SessionState' transition
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
        -- ^ The event handler is called by
        -- 'Postmaster.Main.handleDialog' every time new
        -- \"@\\r\\n@\"-terminated input is available. The
        -- event to trigger is determined by 'smtpdFSM'.
  , dataHandler  :: (Ptr Word8, Int) -> Smtpd ()
        -- ^ Better stay away from this for the time being.
        -- Your data handler of choice is
        -- 'Postmaster.Event.feed'.
  , logStream    :: LogMsg -> IO ()
        -- ^ 'Smtpd' is a 'MonadWriter', but at some point
        -- the (lazily!) accumulated log messages have to go
        -- somewhere. Here you can determine where they go
        -- (or won't go). It's probably best to use
        -- 'Postmaster.Main.syslogger'.
  , queryDNS     :: Resolver
        -- ^ Postmaster uses this call-back to access the
        -- domain name service. Initialize with
        -- 'initResolver'.
  }

-- |Use 'Postmaster.Main.mkConfig' to create a 'Config'
-- record with all values initialized to sensible defaults.

data Config = Config
  { callbacks    :: Callbacks
  , globalEnv    :: MVar Env
        -- ^ A global environment for the entire daemon.
        -- Initialized to 'emptyEnv' by 'Postmaster.Main.withConfig'.
  , sendmailPath :: FilePath
        -- ^ Usually: @\/usr\/sbin\/sendmail@. Expect this
        -- entry to be renamed or to change completely in
        -- the future.
  , peerAddr     :: Maybe SockAddr
        -- ^ set by 'Postmaster.Main.smtpdMain'
  }
  deriving (Show)

-- |Our identifier type.

type ID = Int

-- ** Server State

data SmtpdState = SmtpdState
  { ioBufferGap  :: Int
        -- ^ I\/O driver internal. Don't touch at all.
  , readTimeout  :: Timeout
        -- ^ The default are 90 seconds. You can change the
        -- initial value in the 'Greeting' event, though. If
        -- a timeout occurs, the session aborts with an
        -- exception (which is caught and logged by
        -- 'Postmaster.Main.runSmtpd').
  , writeTimeout :: Timeout
        -- ^ Like 'readTimeout', but for outbound I\/O.
        -- Timeouts are specified in microseconds (@1\/10^6@
        -- seconds).
  , localEnv     :: Env
        -- ^ Transient environment for the TCP-session. May
        -- be initialized to your needs in the 'Greeting'
        -- event.
  }
  deriving (Show)

-- |Used by 'Postmaster.Main.smtpdMain'.

initSmtpd :: SmtpdState
initSmtpd = SmtpdState
  { ioBufferGap  = 0
  , readTimeout  = 90*1000000
  , writeTimeout = 90*1000000
  , localEnv     = FM.empty
  }

-- ** Mail Targets

data Target = Target [Mailbox] MailHandler MailerStatus
            deriving (Show, Typeable)

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
-- contains the current 'SmtpdState' and the \"SessionID\".

yell :: LogEvent -> Smtpd ()
yell e = do
  sid <- local $ fmap (maybe 0 id) (getval "SessionID")
  st <- get
  tell [LogMsg sid st e]

-- |A version of 'yell' which works outside of 'Smtpd'. It
-- writes directly to 'logStream'.

yellIO :: (MonadIO m) => Config -> SmtpdState -> LogEvent -> m ()
yellIO cfg st e = do
  let getSid = fmap (maybe 0 id) (getval "SessionID")
      sid    = fst . runState getSid $ localEnv st
  liftIO ((logStream (callbacks cfg)) (LogMsg sid st e))

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
