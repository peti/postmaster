{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.Base
   Copyright   :  (c) 2005-02-06 by Peter Simons
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
-- import Control.Concurrent
import Control.Monad.RWS hiding ( local )
import Network.DNS
import MonadEnv
import Data.Typeable
import Rfc2821 hiding ( path )
-- import Child
import Postmaster.Extern

-- * The @Smtpd@ Monad

type SmtpdState = Env
type Smtpd a    = RWST GlobalEnv [LogMsg] SmtpdState IO a

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

-- ** Environment

global :: EnvT a -> Smtpd a
global f = ask >>= liftIO . global' f

local :: (MonadState Env m) => EnvT a -> m a
local f = do
  (a, st) <- gets (runState f)
  put st
  return a

type ID = Int

-- |Produce a unique 'ID' using a global counter.

getUniqueID :: Smtpd ID
getUniqueID = global $ tick "UniqueID"

-- |Provides a unique 'ID' for this session.

mySessionID :: Smtpd ID
mySessionID = do
  sid' <- local (getval "SessionID")
  case sid' of
    Just sid -> return sid
    _        -> do sid <- getUniqueID
                   local $ setval "SessionID" sid
                   return sid

-- ** Event Handler

newtype EventHandler = EH (Event -> Smtpd SmtpReply)
                     deriving (Typeable)

-- |If the @EventHandler@ variable is unset in the 'local'
-- environment, the 'global' one will be used.

getEventHandler :: Smtpd (Event -> Smtpd SmtpReply)
getEventHandler = do
  EH f <- local (getval "EventHandler") >>=
            maybe (global $ getval_ "EventHandler") return
  return f

-- |Trigger the given event.

trigger :: Event -> Smtpd SmtpReply
trigger e = getEventHandler >>= ($ e)

-- ** DNS Resolving

newtype DNSResolver = DNSR Resolver
                    deriving (Typeable)

getDNSResolver :: Smtpd Resolver
getDNSResolver =
  global $ getval_ "DNSResolver" >>= return . \(DNSR f) -> f

queryA :: HostName -> Smtpd (Maybe [HostAddress])
queryA h = getDNSResolver >>= \r -> liftIO $ query resolveA r h

queryPTR :: HostAddress -> Smtpd (Maybe [HostName])
queryPTR h = getDNSResolver >>= \r -> liftIO $ query resolvePTR r h

queryMX :: HostName -> Smtpd (Maybe [(HostName, HostAddress)])
queryMX h = getDNSResolver >>= \r -> liftIO $ query resolveMX r h

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

-- ** Logging

data LogMsg = LogMsg ID SmtpdState LogEvent
            deriving (Show)

-- |Given a log event, construct a 'LogMsg' and write it to
-- the monad's log stream with 'tell'. Every log event
-- contains the current 'SmtpdState' and the \"SessionID\".

yell :: LogEvent -> Smtpd ()
yell e = do
  sid <- mySessionID
  st <- get
  tell [LogMsg sid st e]

data LogEvent
  = Msg String
  | StartSession
  | EndSession
  | Input String
  | Output String
  | AcceptConn SockAddr
  | DropConn SockAddr
  | UserShutdown
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

instance Show ExternHandle where
  show _ = "<ExternHandle>"
