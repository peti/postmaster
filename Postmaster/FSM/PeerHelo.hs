{- |
   Module      :  Postmaster.FSM.PeerHelo
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.PeerHelo where

import Control.Monad
import Network ( HostName )
import Postmaster.Base
import Control.Monad.Env
import Text.ParserCombinators.Parsec.Rfc2821

-- |Local Variable: @PEERHELO :: 'HostName'@

peerHelo :: SmtpdVariable
peerHelo = defineLocal "peerhelo"

-- |Set when 'SayHelo' or 'SayEhlo' succeed.

handlePeerHelo :: EventT
handlePeerHelo f e = do
  r <- f e
  case (e, isSuccess r) of
    (SayEhlo peer, True) -> peerHelo (`setVar` peer)
    (SayHelo peer, True) -> peerHelo (`setVar` peer)
    (_, _)               -> return ()
  return r

-- |Will 'fail' when @PEERHELO@ is not set.

getPeerHelo :: Smtpd HostName
getPeerHelo = peerHelo getVar_
