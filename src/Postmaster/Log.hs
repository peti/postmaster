{- |
   Module:      Postmaster
   Copyright:   (C) 2004-2019 Peter Simons
   License:     GNU AFFERO GPL v3 or later

   Maintainer:  simons@cryp.to
   Stability:   experimental
   Portability: non-portable
 -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Postmaster.Log
  ( LogAction(LogAction), HasLogAction(..), LogMsg, MonadLog, logMsg
  , logDebug, logInfo, logWarning, logError
  , logWithPrefix
  , logToHandle, logToSyslog
  )
  where

import Postmaster.Prelude

import Data.ByteString.Builder ( hPutBuilder )
import System.Posix.Syslog

newtype LogAction m msg = LogAction { _runLogAction :: msg -> m ()  }

makeClassy ''LogAction

instance Contravariant (LogAction m) where
  contramap mapMsg (LogAction f) = LogAction (f . mapMsg)

instance Applicative m => Semigroup (LogAction m msg) where
  LogAction lhs <> LogAction rhs = LogAction (\msg -> lhs msg *> rhs msg)

instance Applicative m => Monoid (LogAction m msg) where
  mempty = LogAction (const (pure ()))

type LogMsg = (Priority, Builder)

type MonadLog env m = (MonadReader env m, HasLogAction env m LogMsg)

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
                unsafeUseAsCStringLen (toStrict (toLazyByteString msg)) $
                  syslog Nothing pri

logWithPrefix :: MonadLog env m => Builder -> m a -> m a
logWithPrefix pref = local (over logAction (contramap (over _2 (pref <>))))
