{- |
   Module:      Postmaster.IO
   Copyright:   (C) 2004-2025 Peter Simons
   License:     GNU AFFERO GPL v3 or later

   Maintainer:  simons@cryp.to
   Stability:   experimental
   Portability: non-portable
 -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Postmaster.IO where

import Postmaster.Error
import Postmaster.Log
import Postmaster.Prelude

import Control.Exception ( AssertionFailed(..) )
import Network.Socket
import qualified Network.Socket.ByteString as Socket

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
  if null ais
     then throwIO (AssertionFailed ("getAddrInfo " <> show (host,port) <> " returned no result"))
     else withListenSocket socketHandler (head ais)

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
acceptor socketHandler (listenSocket,listenAddr) =
  enterContext ("listener " <> show listenAddr) $ do
    logInfo "accepting incoming connections"
    forever $
      bracketOnError (liftIO (accept listenSocket)) (liftIO . close . fst) $ \peer@(connSock, connAddr) -> do
        logDebug $ "new incoming connection from " <> display connAddr
        forkIOWithUnmask $ \unmask ->
          unmask (enterContext (show connAddr) (logUncaughtExceptions (socketHandler peer)))
          `finally`
          liftIO (close connSock)

-- | Wrap 'getNameInfo' for a more useful interface. Further details are at
-- https://github.com/haskell/network/issues/416.

resolvePtr :: MonadIO m => SockAddr -> m (Maybe HostName)
resolvePtr addr = liftIO $
  handleIO (const (return Nothing)) $
    fst <$> Network.Socket.getNameInfo [NI_NAMEREQD] True False addr


-- | Show a socket's IP address.
--
-- >>> showAddress (SockAddrInet defaultPort (tupleToHostAddress (1,2,3,4)))
-- "1.2.3.4"
-- >>> showAddress (SockAddrInet6 defaultPort 0 (tupleToHostAddress6 (1,2,3,4,5,6,7,8)) 0)
-- "1:2:3:4:5:6:7:8"

showAddress :: MonadIO m => SockAddr -> m String
showAddress addr = liftIO $
  getNameInfo [NI_NUMERICHOST] True False addr
  >>= maybe (throwString ("getNameInfo failed to format SockAddr " <> show addr)) return . fst


-- | Show a socket address as an ESMTP address literal.
--
-- >>> showAddressLiteral (SockAddrInet defaultPort (tupleToHostAddress (1,2,3,4)))
-- "[1.2.3.4]"
-- >>> showAddressLiteral (SockAddrInet6 defaultPort 0 (tupleToHostAddress6 (1,2,3,4,5,6,7,8)) 0)
-- "[IPv6:1:2:3:4:5:6:7:8]"
-- >>> showAddressLiteral (SockAddrInet6 defaultPort 0 (tupleToHostAddress6 (1,2,0,0,0,6,7,8)) 0)
-- "[IPv6:1:2::6:7:8]"
-- >>> showAddressLiteral (SockAddrInet6 defaultPort 0 (tupleToHostAddress6 (1,2,0,0,5,0,0,8)) 0)
-- "[IPv6:1:2::5:0:0:8]"

showAddressLiteral :: MonadIO m => SockAddr -> m String
showAddressLiteral addr = do
  x <- showAddress addr
  let v6prefix = case addr of SockAddrInet6 {} -> "IPv6:"
                              _                -> ""
  return $ (:) '[' . showString v6prefix . showString x $ "]"
