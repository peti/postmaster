{- |
   Module      :  Postmaster.FSM.EhloPeer
   Copyright   :  (c) 2005-02-09 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.EhloPeer where

import Postmaster.Base
import MonadEnv
import Rfc2821

-- |Local Variable: @EHLOPEER :: 'Bool'@

ehloPeer :: SmtpdVariable
ehloPeer = defineLocal "ehlopeer"

handleEhloPeer :: EventT
handleEhloPeer f e = do
  r <- f e
  case (e, isSuccess r) of
    (SayEhlo _, True) -> ehloPeer (`setval` True)
    (SayHelo _, True) -> ehloPeer (`setval` False)
    (_, _)            -> return ()
  return r

isEhloPeer :: Smtpd Bool
isEhloPeer = ehloPeer (`getDefault` False)
