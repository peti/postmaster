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
