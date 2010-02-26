{- |
   Module      :  Postmaster.Env
   Copyright   :  (c) 2004-2008 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   A generic environment.
-}

module Postmaster.Env where

import Control.Monad.State
import Data.Char
import Data.Dynamic
import Data.ByteString.Char8 ( ByteString, pack )
import qualified Data.Map as FM

-- |Variables are stored in all-uppercase internally. Thus,
-- the environment is case-less.

newtype Variable = Var ByteString
  deriving (Eq, Ord)

instance Show Variable where
  showsPrec _ (Var str) = shows str

-- |Construct a variable.

mkVar :: String -> Variable
mkVar = Var . pack . map toUpper

-- |An environment maps from 'Variable's to any 'Typeable'
-- value. The 'toDyn' and 'fromDynamic' casts are done by
-- the /environment/; the API does not expose 'Dynamic'.

type Env = FM.Map Variable Dynamic

-- |The empty environment.

emptyEnv :: Env
emptyEnv = FM.empty

-- |The environment is accessed through a monadic interface.

type EnvT a = State Env a

-- |Test whether a variable is set.

isVar :: Variable -> EnvT Bool
isVar = gets . FM.member

-- |Set a variable.

setVar :: (Typeable a) => Variable -> a -> EnvT ()
setVar key val = unsetVar key >> modify (FM.insert key (toDyn val))

-- |Erase an entry from the environment.

unsetVar :: Variable -> EnvT ()
unsetVar = modify . FM.delete

-- |This function does not distinguish between the two
-- potential cases of failure we have: (1) value does not
-- exist and (2) value does exist but 'fromDynamic' failed.
-- Accurate type signatures should prevent the latter case
-- from happening anyway.

getVar :: (Typeable a) => Variable -> EnvT (Maybe a)
getVar key = do
  res <- gets $ FM.lookup key
  case res of
    Nothing -> return Nothing
    Just a  -> return (fromDynamic a)

-- |Wrapper that will 'fail' when the variable is unset.

getVar_ :: (Typeable a) => Variable -> EnvT a
getVar_ key = getVar key >>= maybe badluck return
  where badluck = fail ("variable " ++ show key ++ " is unset!")

-- |Wrapper that will return a default when the variable is
-- unset.

getVarDef :: (Typeable a) => Variable -> a -> EnvT a
getVarDef key def = getVar key >>= return . maybe def id

-- |Modify a variable in the environment and return it.
-- 'Nothing' may signify that the value wasn't set or that
-- conversion from 'Dynamic' failed; it's not
-- differentiated.

modifyVar :: (Typeable a) => Variable -> (Maybe a -> (b, a)) -> EnvT b
modifyVar key f = do
  (b, a) <- fmap f (getVar key)
  setVar key a
  return b

-- |Variant that returns @()@. Comes in handy on the occasion.

modifyVar_ :: (Typeable a) => Variable -> (Maybe a -> a) -> EnvT ()
modifyVar_ key f = getVar key >>= setVar key . f

-- |Convenience wrapper to implement counters. Starts
-- counting with @0@ in case the variable doesn't exist or
-- doesn't contain an 'Int'.

tick :: Variable -> EnvT Int
tick key = modifyVar key (maybe (0, 1) (\i -> (i, i+1)))
