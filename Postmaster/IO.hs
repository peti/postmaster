{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.IO
   Copyright   :  (c) 2005-02-06 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.IO where

import Prelude hiding ( catch )
import Data.Maybe
import Data.Typeable
import Control.Exception
import Control.Monad.RWS hiding ( local )
import System.IO
import Postmaster.Base
import Rfc2821
import BlockIO
import Child
import MonadEnv

-- |The exception we throw when writes time out.

data WriteTimeout = WriteTimeout Timeout
                  deriving (Typeable, Show)

setReadTimeout :: Timeout -> Smtpd ()
setReadTimeout = local . setval "ReadTimeout"

getReadTimeout :: Smtpd Timeout
getReadTimeout = local $ getDefault "ReadTimeout" (90 * 1000000)

setWriteTimeout :: Timeout -> Smtpd ()
setWriteTimeout = local . setval "WriteTimeout"

getWriteTimeout :: Smtpd Timeout
getWriteTimeout = local $ getDefault "WriteTimeout" (90 * 1000000)

safeWrite :: IO a -> Smtpd a
safeWrite f = do
  to <- getWriteTimeout
  liftIO $ timeout to f >>= maybe (throwDyn (WriteTimeout to)) return

safeReply :: WriteHandle -> SmtpReply -> Smtpd ()
safeReply hOut r = safeWrite (hPutStr hOut (show r))

safeFlush :: WriteHandle -> Smtpd ()
safeFlush hOut = safeWrite (hFlush hOut)
