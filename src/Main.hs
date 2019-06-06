{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Postmaster

import Data.ByteString.Builder ( hPutBuilder, toLazyByteString )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BS
import Network.Socket hiding ( Debug )
import System.Posix.Syslog as Syslog

type LogMsg = (Priority, Builder)

newtype LogAction m msg = LogAction { _runLogAction :: msg -> m ()  }

makeClassy ''LogAction

instance Contravariant (LogAction m) where
  contramap mapMsg (LogAction f) = LogAction (f . mapMsg)

instance Applicative m => Semigroup (LogAction m msg) where
  LogAction lhs <> LogAction rhs = LogAction (\msg -> lhs msg *> rhs msg)

instance Applicative m => Monoid (LogAction m msg) where
  mempty = LogAction (const (pure ()))

type MonadLog env m = (MonadReader env m, HasLogAction env m LogMsg)

logMsg :: (MonadLog env m) => Priority -> Builder -> m ()
logMsg pri msg = view runLogAction >>= \logger -> logger (pri,msg)

logDebug, logInfo, logWarning, logError :: (MonadLog env m) => Builder -> m ()
logDebug   = logMsg Debug
logInfo    = logMsg Info
logWarning = logMsg Warning
logError   = logMsg Error

logToHandle :: MonadIO m => Handle -> LogAction m LogMsg
logToHandle h = LogAction $ \(_, msg) -> liftIO (hPutBuilder h (msg <> char8 '\n'))

logToSyslog :: MonadIO m => LogAction m LogMsg
logToSyslog = LogAction $ \(pri,msg) -> liftIO $
                BS.unsafeUseAsCStringLen (BSL.toStrict (toLazyByteString msg)) $
                  syslog Nothing pri

data IOState = SocketIO { _hinput :: Handle, _houtput :: Handle }
  deriving (Show)

makeClassy ''IOState

newtype Postmaster a = Postmaster { runPostmaster :: ReaderT (LogAction Postmaster LogMsg) IO a }
  deriving ( Applicative, Functor, Monad, MonadIO, MonadUnliftIO
           , MonadReader (LogAction Postmaster LogMsg)
           )

main :: IO ()
main =
  withSocketsDo $
    withSyslog "postmaster" [] Mail $
      runReaderT (runPostmaster postmaster) ({- logToSyslog <> -} logToHandle stderr)

postmaster :: (MonadUnliftIO m, MonadLog env m) => m ()
postmaster = do
  logDebug "postmaster starting up ..."
  listener (Just "localhost", "2525") (acceptor esmtpd)

esmtpd :: (MonadLog env m) => SocketHandler m
esmtpd sock = do
  let socketId :: Builder
      socketId = display sock <> ": "
  local (over logAction (contramap (over _2 (socketId <>)))) $
    logDebug "Yeah, let's go."

-- * Network Utilities

type SocketHandler m = Socket -> m ()

listener :: (MonadUnliftIO m, MonadLog env m)
         => (Maybe HostName, ServiceName) -> SocketHandler m -> m ()
listener listenAddress@(host,port) socketHandler = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  ais <- liftIO (getAddrInfo (Just hints) host (Just port))
  logDebug $ "listener: " <> display listenAddress <> " resolves to " <> display (map addrAddress ais)
  mapConcurrently_ (withListenSocket socketHandler) ais

withListenSocket :: MonadUnliftIO m => SocketHandler m -> AddrInfo -> m ()
withListenSocket socketHandler ai =
  bracket (liftIO (socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai))) (liftIO . close) $ \sock -> do
    liftIO $ do setSocketOption sock ReuseAddr 1
                withFdSocket sock setCloseOnExecIfNeeded
                bind sock (addrAddress ai)
                listen sock 10
    socketHandler sock

acceptor :: (MonadUnliftIO m, MonadLog env m) => SocketHandler m -> Socket -> m ()
acceptor socketHandler listenSocket = forever $
  bracketOnError (liftIO (accept listenSocket)) (liftIO . close . fst) $ \(connSock, connAddr) -> do
    logDebug $ "accepted connection from " <> display connAddr <> " on " <> display connSock
    forkWithUnmask (\unmask -> unmask (socketHandler connSock) `finally` liftIO (close connSock))
