{- |
   Module:      Postmaster.Prelude
   Copyright:   (C) 2004-2019 Peter Simons
   License:     GNU AFFERO GPL v3 or later

   Maintainer:  simons@cryp.to
   Stability:   experimental
   Portability: non-portable
 -}

module Postmaster.Prelude
  ( module Control.Lens
  , module Control.Monad
  , module Control.Monad.Fail
  , module Control.Monad.IO.Class
  , module Control.Monad.IO.Unlift
  , module Control.Monad.Reader
  , module Control.Monad.State.Strict
  , module Data.ByteString.Builder, display
  , module Data.Either
  , module Data.Maybe
  , module Data.String
  , module Data.Word
  , module System.IO
  , module UnliftIO.Async
  , module UnliftIO.Concurrent
  , module UnliftIO.Exception

  , Text, packText, unpackText, encodeUtf8Text, decodeUtf8Text
  , LazyText, packLazyText, unpackLazyText, encodeUtf8LazyText, decodeUtf8LazyText, BSL.toStrict, BSL.fromStrict
  , ByteString, packBS, packBS8, unpackBS, unpackBS8, unsafeUseAsCStringLen
  , LazyByteString, packLBS, unpackLBS
  ) where

import Prelude hiding ( fail )

import Control.Lens
import Control.Monad hiding ( fail )
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader hiding ( fail )
import Control.Monad.State.Strict hiding ( fail )
import qualified Data.ByteString as BS
import Data.ByteString.Builder ( Builder, char8, charUtf8, string8, stringUtf8, toLazyByteString )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Unsafe ( unsafeUseAsCStringLen )
import Data.Either
import Data.Maybe
import Data.String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import Data.Word
import System.IO hiding ( char8 )
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Exception

display :: Show a => a -> Builder
display = stringUtf8 . show

type ByteString = BS.ByteString

packBS :: [Word8] -> ByteString
packBS = BS.pack

packBS8 :: String -> ByteString
packBS8 = BS8.pack

unpackBS :: ByteString -> [Word8]
unpackBS = BS.unpack

unpackBS8 :: ByteString -> String
unpackBS8 = BS8.unpack

type LazyByteString = BSL.ByteString

packLBS :: [Word8] -> LazyByteString
packLBS = BSL.pack

unpackLBS :: [Word8] -> LazyByteString
unpackLBS = BSL.pack

type Text = Text.Text

packText :: String -> Text
packText = Text.pack

unpackText :: Text -> String
unpackText = Text.unpack

encodeUtf8Text :: Text -> ByteString
encodeUtf8Text = Text.encodeUtf8

decodeUtf8Text :: ByteString -> Text
decodeUtf8Text = Text.decodeUtf8

type LazyText = LText.Text

packLazyText :: String -> LazyText
packLazyText = LText.pack

unpackLazyText :: LazyText -> String
unpackLazyText = LText.unpack

encodeUtf8LazyText :: LazyText -> LazyByteString
encodeUtf8LazyText = LText.encodeUtf8

decodeUtf8LazyText :: LazyByteString -> LazyText
decodeUtf8LazyText = LText.decodeUtf8
