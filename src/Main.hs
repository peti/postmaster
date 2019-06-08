{- |
   Module:      Main
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

module Main where

import Postmaster

import Network.Socket
import System.Exit
import System.Posix.Syslog as Syslog ( withSyslog, Facility(Mail) )
import qualified Data.ByteString as BS

data EsmtpEnv m = EsmtpEnv { _esmtpLogger :: LogAction m LogMsg, _esmtpPeer :: NetworkPeer m }

makeClassy ''EsmtpEnv

instance HasLogAction (EsmtpEnv m) m LogMsg where logAction = esmtpLogger
instance HasNetworkPeer (EsmtpEnv m) m where networkPeer = esmtpPeer

newtype Postmaster a = Postmaster { runPostmaster :: ReaderT (EsmtpEnv Postmaster) IO a }
  deriving ( Applicative, Functor, Monad, MonadIO, MonadUnliftIO
           , MonadReader (EsmtpEnv Postmaster)
           )

newtype MainThread a = MainThread { runMainThread :: ReaderT (LogAction MainThread LogMsg) IO a }
  deriving ( Applicative, Functor, Monad, MonadIO, MonadUnliftIO
           , MonadReader (LogAction MainThread LogMsg)
           )

main :: IO ()
main =
  withSocketsDo $
    withSyslog "postmaster" [] Mail $
      runReaderT (runMainThread postmaster) (logToHandle stderr)

postmaster :: (MonadUnliftIO m, MonadLog env m) => m ()
postmaster =
  handle (\e -> logError ("fatal error: " <> display (e::SomeException)) >> liftIO exitFailure) $ do
    logDebug "postmaster starting up ..."
    listener (Just "0.0.0.0", "2525") (acceptor esmtpd)

esmtpd :: MonadIO m => SocketHandler m
esmtpd (sock,addr) = do
  let run = catch (lineReader mempty) (\e -> logWarning ("uncaught exception: " <> display (e::SomeException)))
      env = EsmtpEnv (logToHandle stderr) (socketIO sock)
  liftIO $ runReaderT (runPostmaster (logWithPrefix (display addr <> ": ") run)) env

lineReader :: (MonadPeer env m, MonadLog env m) => ByteString -> m ()
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
