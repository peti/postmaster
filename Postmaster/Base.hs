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
import Control.Exception
import Control.Monad.RWS hiding ( local )
import Network.DNS
import MonadEnv
import Data.Typeable
import Rfc2821 hiding ( path )

-- * The @Smtpd@ Monad

type SmtpdState = Env
type Smtpd a    = RWST GlobalEnv [LogMsg] SmtpdState IO a

-- |@say a b c msg = return ('reply' a b c [msg])@
--
-- The 'SmtpReply' codes returned the event handler returns
-- determine what Postmaster will do:
--
-- [@1xx@, @2xx@, @3xx@] make the 'SessionState' transition
-- determined determined by 'smtpdFSM'.
--
-- [@4xx@, @5xx@] Do /not/ make the transition.
--
-- [@221@, @421@] Drop the connection after this reply.
--
-- The reply for the 'Greeting' event (the first event
-- triggered when a session starts up) is interpreted as
-- follows: @2xx@ accepts the connection, everything else
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
getUniqueID = global $ tick (mkVar "UniqueID")

-- |Provides a unique 'ID' for this session.

mySessionID :: Smtpd ID
mySessionID = do
  let key = mkVar "SessionID"
  sid' <- local $ getval key
  case sid' of
    Just sid -> return sid
    _        -> do sid <- getUniqueID
                   local (setval key sid)
                   return sid

-- ** Event Handler

newtype EventHandler = EH (Event -> Smtpd SmtpReply)
                     deriving (Typeable)

-- |If the @EventHandler@ variable is unset in the 'local'
-- environment, the 'global' one will be used.

getEventHandler :: Smtpd (Event -> Smtpd SmtpReply)
getEventHandler = do
  let key = mkVar "EventHandler"
  EH f <- local (getval key)
      >>= maybe (global $ getval_ key) return
  return f

-- |Trigger the given event.

trigger :: Event -> Smtpd SmtpReply
trigger e = getEventHandler >>= ($ e)

-- ** DNS Resolving

newtype DNSResolver = DNSR Resolver
                    deriving (Typeable)

getDNSResolver :: Smtpd Resolver
getDNSResolver =
  global $ getval_ (mkVar "DNSResolver") >>= return . \(DNSR f) -> f

queryA :: HostName -> Smtpd (Maybe [HostAddress])
queryA h = getDNSResolver >>= \r -> liftIO $ query resolveA r h

queryPTR :: HostAddress -> Smtpd (Maybe [HostName])
queryPTR h = getDNSResolver >>= \r -> liftIO $ query resolvePTR r h

queryMX :: HostName -> Smtpd (Maybe [(HostName, HostAddress)])
queryMX h = getDNSResolver >>= \r -> liftIO $ query resolveMX r h

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
  deriving (Show)
