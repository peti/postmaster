{- |
   Module      :  Postmaster
   Copyright   :  (c) 2004-2008 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   The Postmaster ESMTP Server. See
   <http://postmaster.cryp.to/docs/tutorial.html> for an
   introduction.
 -}

module Postmaster
  ( module Postmaster.Base
  , module Postmaster.FSM
  , module Postmaster.IO
  , module Postmaster.Main
  , module Control.Monad.RWS
  , module Data.Typeable
  , module Network
  , module ADNS
  , module Text.ParserCombinators.Parsec.Rfc2821
  , module System.Posix.Syslog
  )
  where

import Network ( PortID(..) )
import Control.Monad.RWS hiding ( local )
import ADNS hiding ( Debug, queryMX, queryA, queryPTR )
import Data.Typeable
import Text.ParserCombinators.Parsec.Rfc2821 hiding ( path )
import System.Posix.Syslog
import Postmaster.Base
import Postmaster.FSM
import Postmaster.IO
import Postmaster.Main

-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-program-name: "ghci -ladns -lcrypto" ***
-- End: ***
