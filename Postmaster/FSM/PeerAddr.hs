{- |
   Module      :  Postmaster.FSM.PeerAddr
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.PeerAddr where

import Postmaster.Base
import Network.Socket ( SockAddr )
import Control.Monad.Env

peerAddr :: Variable
peerAddr = mkVar "peeradr"

setPeerAddr :: SockAddr -> EnvT ()
setPeerAddr = setval peerAddr

getPeerAddr :: Smtpd (Maybe SockAddr)
getPeerAddr = local (getval peerAddr)
