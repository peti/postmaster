{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import Postmaster

import Network.Socket
import qualified Network.Socket.ByteString as Socket
import System.Exit
import System.Posix.Syslog as Syslog ( withSyslog, Facility(Mail) )
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

data EsmtpEnv m = EsmtpEnv { _esmtpLogger :: LogAction m LogMsg, _esmtpPeer :: NetworkPeer m }

makeClassy ''EsmtpEnv

instance HasLogAction (EsmtpEnv m) m LogMsg where logAction = esmtpLogger
instance HasNetworkPeer (EsmtpEnv m) m where networkPeer = esmtpPeer

newtype Postmaster a = Postmaster { runPostmaster :: ReaderT (EsmtpEnv Postmaster) IO a }
  deriving ( Applicative, Functor, Monad, MonadIO, MonadUnliftIO
           , MonadReader (EsmtpEnv Postmaster)
           )

main :: IO ()
main =
  withSocketsDo $
    withSyslog "postmaster" [] Mail $
      runReaderT (runPostmaster postmaster) $
        EsmtpEnv (logToHandle stderr) undefined


postmaster :: (MonadUnliftIO m, MonadLog env m, MonadPeer env m) => m ()
postmaster =
  handle (\e -> logError ("fatal error: " <> display (e::SomeException)) >> liftIO exitFailure) $ do
    logDebug "postmaster starting up ..."
    listener (Just "0.0.0.0", "2525") (acceptor esmtpd)

esmtpd :: (MonadUnliftIO m, MonadLog env m, MonadPeer env m) => SocketHandler m
esmtpd (sock,addr) =
  handle (\e -> logWarning (display (e::SomeException))) $
    enterContext (show addr) $ do
     (r, _) <- liftIO $ getNameInfo [] True False addr
     logDebug $ maybe "cannot resolve peer address" (mappend (string8 "peer has DNS name ") . display) r
     local (set networkPeer (socketIO sock)) (lineReader mempty)

-- * Network Utilities

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

-- * Error Handling Utilities

data ErrorContext = ErrorContext String SomeException
  deriving (Typeable)

instance Show ErrorContext where
  showsPrec _ (ErrorContext ctx e) = showString ctx . showString ": " . showString (displayException e)

instance Exception ErrorContext where

errorContext :: MonadUnliftIO m => String -> m a -> m a
errorContext ctx = handle (throwIO . ErrorContext ctx)

enterContext :: (MonadUnliftIO m, MonadLog env m) => String -> m a -> m a
enterContext ctx = errorContext ctx . logWithPrefix (string8 ctx <> ": ")
