{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   The Postmaster ESMTP Server. See
   <http://postmaster.cryp.to/docs/tutorial.html> for an
   introduction. If there is something that appears to be
   impossible to do without modifying the core modules,
   please complain loudly to <mailto:postmaster-dev@lists.cryp.to>.
 -}

module Postmaster
  ( module Postmaster.Base
  , module Postmaster.FSM
  , module Postmaster.IO
  , module Postmaster.Main
  , module Control.Monad.RWS
  , module Data.Typeable
  , module Network
  , module Network.DNS
  , module System.IO.Driver
  , module MonadEnv
  , module Text.ParserCombinators.Parsec.Rfc2821
  , module Syslog
  )
  where

import Network ( PortID(..) )
import Control.Monad.RWS hiding ( local )
import Network.DNS hiding ( Debug )
import Data.Typeable
import MonadEnv
import Text.ParserCombinators.Parsec.Rfc2821 hiding ( path )
import Syslog
import System.IO.Driver ( Capacity )
import Postmaster.Base
import Postmaster.FSM
import Postmaster.IO
import Postmaster.Main

-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-program-name: "ghci -ladns -lcrypto" ***
-- End: ***
