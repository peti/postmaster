{-# OPTIONS -fth -ddump-splices #-}
{- |
   Module      :  Postmaster.Meta
   Copyright   :  (c) 2005-02-13 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   Wildly experimental stuff.
 -}

module Postmaster.Meta where

import Language.Haskell.TH hiding ( global )
import Control.Monad.Env
import Postmaster.Base

mkBody :: ExpQ -> [ClauseQ]
mkBody e = [ clause [] (normalB e) [] ]

maybeT :: TypeQ -> TypeQ
maybeT = appT (conT (mkName "Maybe"))

maybeAtoA :: TypeQ -> TypeQ
maybeAtoA a = arrowT `appT` ((arrowT `appT` maybeT a) `appT` a)

smtpdT :: TypeQ -> TypeQ
smtpdT = appT (conT (mkName "Smtpd"))

defineVar :: ExpQ -> String -> TypeQ -> [Accessor a] -> Q [Dec]
defineVar f n a accs = do
  modVar <- sequence
    [ sigD (mkName ("modify" ++ n)) (maybeAtoA a `appT` smtpdT a)
    , funD (mkName ("modify" ++ n)) (mkBody [| \g -> $f (modifyVar (mkVar n) g) |])
    , sigD (mkName ("set"    ++ n)) (arrowT `appT` a `appT` [t| Smtpd () |])
    , funD (mkName ("set"    ++ n)) (mkBody [| $f . (setVar (mkVar n)) |])
    , sigD (mkName ("unset"  ++ n)) [t| Smtpd () |]
    , funD (mkName ("unset"  ++ n)) (mkBody [| $f (unsetVar (mkVar n)) |])
    ]
  accVar <- mapM mkAcc accs
  return (modVar ++ concat accVar)
    where
    mkAcc (accName, accf) = sequence
      [ sigD (mkName . accName $ n) (smtpdT a)
      , funD (mkName . accName $ n) (mkBody (varE (mkName ("modify" ++ n)) `appE` accf))
      ]

type Accessor a = (String -> String, ExpQ)

defineLocalVar :: String -> TypeQ -> [Accessor a] -> Q [Dec]
defineLocalVar = defineVar [| local |]

defineGlobalVar :: String -> TypeQ -> [Accessor a] -> Q [Dec]
defineGlobalVar = defineVar [| global |]

mkSig :: [TypeQ] -> TypeQ
mkSig = foldr1 (\lhs rhs -> appT (appT arrowT lhs) rhs)
