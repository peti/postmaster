{-# OPTIONS -fth -ddump-splices #-}
{- |
   Module      :  Postmaster.Meta
   Copyright   :  (c) 2005-02-12 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   This module provides the function 'defineVar' which can
   be used to generate the usual set of access functions for
   a local variable. The invocation

   >   $(defineVar "foo" [t| Int |])

   generates these functions:

   > getFoo :: Smtpd (Maybe Int)
   > getFoo = local (getval (mkVar "foo"))
   >
   > getFoo_ :: Smtpd Int
   > getFoo_ = local (getval_ (mkVar "foo"))
   >
   > setFoo :: Int -> Smtpd ()
   > setFoo = local . setval (mkVar "foo")
   >
   > unsetFoo :: Smtpd ()
   > unsetFoo = local (unsetval (mkVar "foo"))

 -}

module Postmaster.Meta where

import Language.Haskell.TH
import Data.Char
import Control.Monad.Env
import Postmaster.Base

mkBody :: ExpQ -> [ClauseQ]
mkBody e = [ clause [] (normalB e) [] ]

maybeT :: TypeQ -> TypeQ
maybeT = appT (conT (mkName "Maybe"))

smtpdT :: TypeQ -> TypeQ
smtpdT = appT (conT (mkName "Smtpd"))

getN, getN_, setN, unsetN :: String -> Name
getN   n = mkName $ "get"   ++ toUpper (head n) : tail n
getN_  n = mkName $ "get"   ++ toUpper (head n) : tail n ++ "_"
setN   n = mkName $ "set"   ++ toUpper (head n) : tail n
unsetN n = mkName $ "unset" ++ toUpper (head n) : tail n

getS, getS_, setS, unsetS :: TypeQ -> String -> DecQ
getS   t n = sigD (getN   n) (smtpdT . maybeT $ t)
getS_  t n = sigD (getN_  n) (smtpdT $ t)
setS   t n = sigD (setN   n) ((arrowT `appT` t) `appT` [t| Smtpd () |])
unsetS _ n = sigD (unsetN n) [t| Smtpd () |]

getB, getB_, setB, unsetB :: String -> DecQ
getB   n = funD (getN   n) (mkBody [| local (getval  (mkVar n))  |])
getB_  n = funD (getN_  n) (mkBody [| local (getval_ (mkVar n))  |])
setB   n = funD (setN   n) (mkBody [| local . setval (mkVar n)   |])
unsetB n = funD (unsetN n) (mkBody [| local (unsetval (mkVar n)) |])

defineVar :: String -> TypeQ -> Q [Dec]
defineVar n t = mapM (\f -> f n) [ getS   t, getB
                                 , getS_  t, getB_
                                 , setS   t, setB
                                 , unsetS t, unsetB
                                 ]
