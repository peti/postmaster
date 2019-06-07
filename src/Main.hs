{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Postmaster

import Network.Socket hiding ( Debug )
import System.Posix.Syslog as Syslog

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
