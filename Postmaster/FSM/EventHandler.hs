{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.FSM.EventHandler
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.EventHandler where

import Data.Typeable
import Postmaster.Base
import Control.Monad.Env
import Text.ParserCombinators.Parsec.Rfc2821

newtype EH = EH EventHandler
           deriving (Typeable)

-- |This variable /must/ be initialized in the global
-- environment or Postmaster won't do much.

eventHandler :: Variable
eventHandler = mkVar "eventhandler"

setEventHandler :: EventHandler -> EnvT ()
setEventHandler = setVar eventHandler . EH

getEventHandler :: Smtpd EventHandler
getEventHandler = do
  EH f <- local (getVar eventHandler)
      >>= maybe (global $ getVar_ eventHandler) return
  return f

-- |Trigger the given event.

trigger :: Event -> Smtpd SmtpReply
trigger e = getEventHandler >>= ($ e)
