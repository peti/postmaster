{- |
   Module:      Postmaster
   Copyright:   (C) 2004-2019 Peter Simons
   License:     GNU AFFERO GPL v3 or later

   Maintainer:  simons@cryp.to
   Stability:   experimental
   Portability: non-portable
 -}

module Postmaster
  ( module Control.Lens
  , module Control.Monad
  , module Control.Monad.Fail
  , module Control.Monad.IO.Class
  , module Control.Monad.IO.Unlift
  , module Control.Monad.Reader
  , module Control.Monad.State
  , module Data.ByteString.Builder, display
  , module Data.String
  , module Data.Text
  , module Data.Word
  , module System.IO
  , module UnliftIO.Async
  , module UnliftIO.Concurrent
  , module UnliftIO.Exception
  ) where

import Prelude hiding ( fail )

import Control.Lens
import Control.Monad hiding ( fail )
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader hiding ( fail )
import Control.Monad.State hiding ( fail )
import Data.ByteString.Builder ( Builder, char8, charUtf8, string8, stringUtf8 )
import Data.String
import Data.Text ( Text )
import System.IO hiding ( char8 )
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception
import Data.Word

display :: Show a => a -> Builder
display = stringUtf8 . show
