{- |
   Module:      Postmaster.IO
   Copyright:   (C) 2004-2019 Peter Simons
   License:     GNU AFFERO GPL v3 or later

   Maintainer:  simons@cryp.to
   Stability:   experimental
   Portability: non-portable
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Postmaster.IO where

import Postmaster.Error
import Postmaster.Log
import Postmaster.Prelude

import Network.Socket
import qualified Network.Socket.ByteString as Socket
import qualified Data.ByteString as BS

data NetworkPeer m = NetworkPeer { _readFromPeer :: Word16 -> m ByteString, _sendToPeer :: ByteString -> m () }

makeClassy ''NetworkPeer

socketIO :: MonadIO m => Socket -> NetworkPeer m
socketIO s = NetworkPeer (liftIO . Socket.recv s . fromIntegral) (liftIO . Socket.sendAll s)

type MonadPeer env m = (MonadReader env m, HasNetworkPeer env m)

recv :: (MonadPeer env m) => Word16 -> m ByteString
recv len = view readFromPeer >>= \f -> f len

send :: (MonadPeer env m) => ByteString -> m ()
send buf = view sendToPeer >>= \f -> f buf

type SocketHandler m = (Socket, SockAddr) -> m ()

listener :: MonadUnliftIO m => (Maybe HostName, ServiceName) -> SocketHandler m -> m ()
listener (host,port) socketHandler = do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  ais <- liftIO (getAddrInfo (Just hints) host (Just port))
  mapConcurrently_ (withListenSocket socketHandler) ais

withListenSocket :: MonadUnliftIO m => SocketHandler m -> AddrInfo -> m ()
withListenSocket socketHandler ai =
  errorContext (show (addrAddress ai)) $
    bracket (liftIO (socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai))) (liftIO . close) $ \sock -> do
      liftIO $ do setSocketOption sock ReuseAddr 1
                  withFdSocket sock setCloseOnExecIfNeeded
                  bind sock (addrAddress ai)
                  listen sock 10
      socketHandler (sock, addrAddress ai)

acceptor :: (MonadUnliftIO m, MonadLog env m) => SocketHandler m -> SocketHandler m
acceptor socketHandler (listenSocket,listenAddr) = do
  let socketId = "listener " <> display listenAddr <> ": "
  logInfo $ socketId <> "accepting incoming connections"
  forever $
    bracketOnError (liftIO (accept listenSocket)) (liftIO . close . fst) $ \(connSock, connAddr) -> do
      logDebug $ socketId <> "new incoming connection from " <> display connAddr
      forkIOWithUnmask (\unmask -> unmask (socketHandler (connSock,connAddr)) `finally` liftIO (close connSock))

lineReader :: (MonadIO m, MonadPeer env m, MonadLog env m) => ByteString -> m ()
lineReader buf = do
  let maxLineLength = 4096              -- TODO: magic constant
      maxReadSize = maxLineLength - BS.length buf
  if maxReadSize <= 0
     then logWarning $ "client exceeded maximum line length (" <> display maxLineLength <> " characters)"
     else do new <- recv (fromIntegral maxReadSize)
             logDebug $ "recv: " <> display new
             if BS.null new
                then logDebug $ "reached end of input (left-over in buffer: " <> display buf <> ")"
                else case BS.breakSubstring "\r\n" (buf <> new) of
                       (line,rest) | BS.null rest -> do logDebug $ "no complete line yet (buffer: " <> display line <> ")"
                                                        lineReader line
                                   | otherwise    -> do logDebug $ "read line: " <> display line
                                                        lineReader (BS.drop 2 rest)
