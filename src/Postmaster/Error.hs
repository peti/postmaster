{- |
   Module:      Postmaster.Error
   Copyright:   (C) 2004-2019 Peter Simons
   License:     GNU AFFERO GPL v3 or later

   Maintainer:  simons@cryp.to
   Stability:   experimental
   Portability: non-portable
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Postmaster.Error where

import Postmaster.Prelude
import Postmaster.Log

data ErrorContext = ErrorContext String SomeException
  deriving (Typeable)

instance Show ErrorContext where
  showsPrec _ (ErrorContext ctx e) = showString ctx . showString ": " . showString (displayException e)

instance Exception ErrorContext where

errorContext :: MonadUnliftIO m => String -> m a -> m a
errorContext ctx = handle (throwIO . ErrorContext ctx)

enterContext :: (MonadUnliftIO m, MonadLog env m) => String -> m a -> m a
enterContext ctx = errorContext ctx . logWithPrefix (string8 ctx <> ": ")

logUncaughtExceptions :: (MonadUnliftIO m, MonadLog env m) => m () -> m ()
logUncaughtExceptions = handle (\e -> logError ("uncaught exception: " <> display (e::SomeException)))
