{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster
   Copyright   :  (c) 2005-02-06 by Peter Simons
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
  , module Postmaster.Event
  , module Postmaster.IO
  , module Postmaster.Main
  , module Control.Monad.RWS
  , module Data.Typeable
  , module Network
  , module Network.BSD
  , module Network.DNS
  , module Network.Socket
  , module BlockIO
  , module MonadEnv
  , module Rfc2821
  , module Syslog
  )
  where

import Network ( PortID(..) )
import Network.BSD ( getHostName )
import Network.Socket hiding ( listen, shutdown, Debug, send, HostAddress )
import Control.Monad.RWS hiding ( local )
import Network.DNS hiding ( Debug )
import Data.Typeable
import MonadEnv
import Rfc2821 hiding ( path )
import Syslog
import BlockIO ( Capacity )
import Postmaster.Base
import Postmaster.Event
import Postmaster.IO
import Postmaster.Main

-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-program-name: "ghci -ladns -lcrypto" ***
-- End: ***
