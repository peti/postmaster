{- |
   Module      :  Postmaster.FSM.Announce
   Copyright   :  (c) 2005-02-13 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   Announce ESMTP Capability.
 -}

module Postmaster.FSM.Announce where

import Postmaster.Base
import Text.ParserCombinators.Parsec.Rfc2821

-- |Append the given ESMTP keyword to the reply produced
-- during 'SayEhlo'.

announce :: String -> EventT
announce keyword f e@(SayEhlo _) = do
  Reply rc msg <- f e
  let msg' = msg ++ [keyword]
  case rc of
    Code Success _ _ -> return (Reply rc msg')
    _                -> return (Reply rc msg)
announce _ f e = f e
