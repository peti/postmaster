{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{- |
   Module      :  Postmaster.FSM.SessionState
   Copyright   :  (c) 2004-2008 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.SessionState where

import Postmaster.Base
import Text.Parsec.Rfc2821
import Data.Typeable

newtype SmtpSessionState = SSST SessionState
                           deriving (Typeable)

-- |Local Variable: @SESSIONSTATE :: 'SessionState'@

sessionState :: SmtpdVariable
sessionState = defineLocal "sessionstate"

setSessionState :: SessionState -> Smtpd ()
setSessionState sst = sessionState (`setVar` (SSST sst))

getSessionState :: Smtpd SessionState
getSessionState = sessionState (`getVarDef` (SSST Unknown)) >>= \(SSST sst) -> return sst
