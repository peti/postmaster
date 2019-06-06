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
import Network.DNS
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

logWithPrefix :: MonadLog env m => Builder -> m a -> m a
logWithPrefix pref = local (over logAction (contramap (over _2 (pref <>))))

logMsg :: MonadLog env m => Priority -> Builder -> m ()
logMsg pri msg = view runLogAction >>= \logger -> logger (pri,msg)

logDebug, logInfo, logWarning, logError :: MonadLog env m => Builder -> m ()
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
  deriving ( Applicative, Functor, Monad, MonadIO, MonadFail, MonadUnliftIO
           , MonadReader (LogAction Postmaster LogMsg)
           )

main :: IO ()
main =
  withSocketsDo $
    withSyslog "postmaster" [] Mail $
      runReaderT (runPostmaster postmaster) ({- logToSyslog <> -} logToHandle stderr)

postmaster :: (MonadUnliftIO m, MonadFail m, MonadLog env m) => m ()
postmaster = do
  logDebug "postmaster starting up ..."
  listener (Just "0.0.0.0", "2525") (acceptor esmtpd)

esmtpd :: (MonadIO m, MonadFail m, MonadLog env m) => SocketHandler m
esmtpd (peer,peerAddr) = logWithPrefix (display peerAddr <> ": ") $ do
  (r, Nothing) <- liftIO $ getNameInfo [] True False peerAddr
  logDebug $ maybe "cannot resolve peer address" (mappend (string8 "peer has DNS name ") . display) r

  return ()

-- * Network Utilities

type SocketHandler m = (Socket, SockAddr) -> m ()

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
    socketHandler (sock, addrAddress ai)

acceptor :: (MonadUnliftIO m, MonadLog env m) => SocketHandler m -> SocketHandler m
acceptor socketHandler (listenSocket,listenAddr) = forever $
  bracketOnError (liftIO (accept listenSocket)) (liftIO . close . fst) $ \(connSock, connAddr) -> do
    logDebug $ "listener " <>  display listenAddr <> ": new incoming connection from " <> display connAddr
    forkWithUnmask (\unmask -> unmask (socketHandler (connSock,connAddr)) `finally` liftIO (close connSock))
