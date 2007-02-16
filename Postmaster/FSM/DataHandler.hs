{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.FSM.DataHandler
   Copyright   :  (c) 2007-02-02 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.DataHandler where

import Data.Typeable
import Postmaster.Base

newtype DH = DH DataHandler
           deriving (Typeable)

dataHandler :: SmtpdVariable
dataHandler = defineLocal "datahandler"

setDataHandler :: DataHandler -> Smtpd ()
setDataHandler f = dataHandler (`setVar` DH f)

getDataHandler :: Smtpd DataHandler
getDataHandler = dataHandler getVar_ >>= \(DH f) -> return f

feed :: DataHandler
feed buf = getDataHandler >>= ($ buf)
