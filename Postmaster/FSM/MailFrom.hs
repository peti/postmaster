{-# OPTIONS -fth #-}
{- |
   Module      :  Postmaster.FSM.MailFrom
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.MailFrom where

import Postmaster.Base
import Postmaster.Meta
import Control.Monad.Env
import Text.ParserCombinators.Parsec.Rfc2821

$( defineLocalVar "MailFrom" [t| Mailbox |]
     [ (showString "get", [| maybe nullPath id |])
     ]
 )

handleMailFrom :: EventT
handleMailFrom f e = do
  r <- f e
  case (e, isSuccess r) of
    (SetMailFrom x, True) -> setMailFrom x
    (ResetState, _)       -> unsetMailFrom
    (_, _)                -> return ()
  return r
