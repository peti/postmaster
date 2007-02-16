{- |
   Module      :  Postmaster.FSM.PeerAddr
   Copyright   :  (c) 2007-02-02 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.PeerAddr where

import Postmaster.Base
import Network.Socket ( SockAddr )

peerAddr :: Variable
peerAddr = mkVar "peeradr"

setPeerAddr :: SockAddr -> EnvT ()
setPeerAddr = setVar peerAddr

getPeerAddr :: Smtpd (Maybe SockAddr)
getPeerAddr = local (getVar peerAddr)
