{- |
   Module      :  Postmaster.FSM.EhloPeer
   Copyright   :  (c) 2005-02-13 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.EhloPeer where

import Postmaster.Base
import Control.Monad.Env
import Text.ParserCombinators.Parsec.Rfc2821

-- |Local Variable: @EHLOPEER :: 'Bool'@

ehloPeer :: SmtpdVariable
ehloPeer = defineLocal "ehlopeer"

handleEhloPeer :: EventT
handleEhloPeer f e = do
  r <- f e
  case (e, isSuccess r) of
    (SayEhlo _, True) -> ehloPeer (`setVar` True)
    (SayHelo _, True) -> ehloPeer (`setVar` False)
    (_, _)            -> return ()
  return r

isEhloPeer :: Smtpd Bool
isEhloPeer = ehloPeer (`getVarDef` False)
