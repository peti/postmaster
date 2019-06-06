{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import Postmaster

import Colog.Core.Action
import Colog.Core.Class
import Colog.Core.Severity as Colog
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BS
import Network.Socket hiding ( Debug )
import System.Posix.Syslog as Syslog

type LogMsg = (Severity, Builder)

type MonadLog env m = (MonadReader env m, HasLog env LogMsg m)

logMsg :: MonadLog env m => Severity -> Builder -> m ()
logMsg pri msg = asks getLogAction >>= \(LogAction f) -> f (pri, msg)

logDebug, logInfo, logWarning, logError :: (MonadLog env m) => Builder -> m ()
logDebug   = logMsg Colog.Debug
logInfo    = logMsg Colog.Info
logWarning = logMsg Colog.Warning
logError   = logMsg Colog.Error

logToHandle :: MonadIO m => Handle -> LogAction m LogMsg
logToHandle h = LogAction $ \(_, msg) -> liftIO (hPutBuilder h (msg <> string8 "\n"))

logToSyslog :: MonadIO m => LogAction m LogMsg
logToSyslog = LogAction $ \(pri,msg) -> liftIO $
                BS.unsafeUseAsCStringLen (BSL.toStrict (toLazyByteString msg)) $
                  syslog Nothing (mapPrio pri)
  where
    mapPrio Colog.Debug   = Syslog.Debug
    mapPrio Colog.Info    = Syslog.Info
    mapPrio Colog.Warning = Syslog.Warning
    mapPrio Colog.Error   = Syslog.Error

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
  listener (Just "localhost", "2525") (acceptor (const (return ())))

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

acceptor :: (MonadUnliftIO m, MonadReader env m, HasLog env LogMsg m) => SocketHandler m -> Socket -> m ()
acceptor socketHandler listenSocket = forever $
  bracketOnError (liftIO (accept listenSocket)) (liftIO . close . fst) $ \(connSock, connAddr) -> do
    logDebug $ "accepted connection from " <> display connAddr
    forkWithUnmask (\unmask -> (unmask ((socketHandler connSock))) `finally` liftIO (close connSock))
