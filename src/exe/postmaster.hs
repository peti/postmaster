{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

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
  deriving ( Applicative, Functor, Monad, MonadIO
           , MonadReader (LogAction Postmaster LogMsg)
           )

main :: IO ()
main =
  withSocketsDo $
    withSyslog "postmaster" [] Mail $
      runReaderT (runPostmaster postmaster) ({- logToSyslog <> -} logToHandle stderr)

postmaster :: MonadLog env m => m ()
postmaster =
  logDebug "postmaster starting up ..."

makeListenSocket :: AddrInfo -> IO Socket
makeListenSocket ai = do
  sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  bind sock (addrAddress ai)
  listen sock 10
  return sock

resolveListenAddr :: ServiceName -> IO AddrInfo
resolveListenAddr port = do
   let hints = defaultHints { addrFlags = [AI_PASSIVE]
                            , addrSocketType = Stream
                            }
   ai:_ <- getAddrInfo (Just hints) Nothing (Just port)
   return ai
