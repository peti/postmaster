{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster
   Copyright   :  (c) 2005-02-03 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   The Postmaster ESMTP Server. See
   <http://postmaster.cryp.to/docs/tutorial.html> for an
   introduction. Note that you should /not/ modify this
   module. In theory, you should be able to achieve any
   effect you like through modifying the contents of
   'Config'. If there is something that appears to be
   impossible to do without modifying the core modules,
   please complain loudly to <mailto:postmaster-dev@lists.cryp.to>.
 -}

module Postmaster
  ( module Postmaster.Base
  , module Rfc2821
  , module Network.DNS
  , module MonadEnv
  , module Control.Monad.RWS
  , module Network
  , module Data.Typeable
  , module Syslog
  , module Network.BSD
  , module Network.Socket
  )
  where

import Network ( PortID(..) )
import Network.BSD ( getHostName )
import Network.Socket hiding ( listen, shutdown, Debug, send, HostAddress )
-- import System.Exit ( ExitCode(..) )
-- import System.Process
-- import Control.Exception
-- import Control.Concurrent
import Control.Monad.RWS hiding ( local )
-- import Data.Unique
-- import qualified Data.Map as FM
import Network.DNS hiding ( Debug )
import Data.Typeable
import MonadEnv
import Rfc2821 hiding ( path )
-- import BlockIO hiding ( loop )
-- import Child ( timeout, Timeout )
import Syslog
import Postmaster.Base


-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-ghci-program-args: ( "-ladns" "-lcrypto" ) ***
-- End: ***
