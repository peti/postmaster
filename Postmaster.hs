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
  , module Network.DNS
  , module Text.ParserCombinators.Parsec.Rfc2821
  , module Syslog
  )
  where

import Network ( PortID(..) )
import Control.Monad.RWS hiding ( local )
import Network.DNS hiding ( Debug )
import Data.Typeable
import Text.ParserCombinators.Parsec.Rfc2821 hiding ( path )
import Syslog
import Postmaster.Base
import Postmaster.FSM
import Postmaster.IO
import Postmaster.Main

-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-program-name: "ghci -ladns -lcrypto" ***
-- End: ***
