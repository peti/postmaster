{- |
   Module      :  Postmaster.FSM.SessionState
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.SessionState where

import Postmaster.Base
import Control.Monad.Env
import Text.ParserCombinators.Parsec.Rfc2821

-- |Local Variable: @SESSIONSTATE :: 'SessionState'@

sessionState :: SmtpdVariable
sessionState = defineLocal "sessionstate"

setSessionState :: SessionState -> Smtpd ()
setSessionState sst = sessionState (`setval` sst)

getSessionState :: Smtpd SessionState
getSessionState = sessionState (`getDefault` Unknown)
