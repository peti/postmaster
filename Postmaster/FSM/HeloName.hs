{- |
   Module      :  Postmaster.FSM.HeloName
   Copyright   :  (c) 2007-02-02 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.HeloName where

import Control.Monad
import Network ( HostName )
import Postmaster.Base
import Text.ParserCombinators.Parsec.Rfc2821

-- |Local Variable: @HELONAME :: 'HostName'@

heloName :: SmtpdVariable
heloName = defineLocal "heloname"

-- |Initialized during the 'Greeting' event; will not
-- overwrite the variable if it does exist already.

initHeloName :: HostName -> EventT
initHeloName n f e = do
  when (e == Greeting) (heloName (`modifyVar_` maybe n id))
  f e

-- |Will 'fail' when @HELONAME@ is not set.

myHeloName :: Smtpd HostName
myHeloName = heloName getVar_

