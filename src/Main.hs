{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Postmaster

import Network.Socket
import Network.Socket.ByteString
import System.Exit
import System.Posix.Syslog as Syslog ( withSyslog, Facility(Mail) )
import qualified Data.ByteString as BS

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
postmaster =
  handle (\e -> logError ("fatal error: " <> display (e::SomeException)) >> liftIO exitFailure) $ do
    logDebug "postmaster starting up ..."
    listener (Just "0.0.0.0", "2525") (acceptor esmtpd)

esmtpd :: (MonadUnliftIO m, MonadFail m, MonadLog env m) => SocketHandler m
esmtpd peer@(sock,addr) =
  handle (\e -> logWarning (display (e::SomeException))) $
    enterContext (show addr) $ do
     (r, Nothing) <- liftIO $ getNameInfo [] True False addr
     logDebug $ maybe "cannot resolve peer address" (mappend (string8 "peer has DNS name ") . display) r
     lineReader mempty peer

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
      forkWithUnmask (\unmask -> unmask (socketHandler (connSock,connAddr)) `finally` liftIO (close connSock))

lineReader :: (MonadIO m, MonadLog env m) => ByteString -> SocketHandler m
lineReader buf peer@(sock,_) = do
  new <- liftIO (recv sock 16)
  logDebug $ "recv: " <> display new
  if BS.null new
     then logDebug $ "reached end of input (left-over in buffer: " <> display buf <> ")"
     else case BS.breakSubstring "\r\n" (buf <> new) of
            (line,rest) | BS.null rest -> do logDebug $ "no complete line yet (buffer: " <> display line <> ")"
                                             lineReader line peer
                        | otherwise    -> do logDebug $ "read line: " <> display line
                                             lineReader (BS.drop 2 rest) peer

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
