{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.FSM.DNSResolver
   Copyright   :  (c) 2005-02-09 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.DNSResolver where

import Control.Monad.RWS hiding ( local )
import Network.DNS
import Data.Typeable
import Postmaster.Base
import MonadEnv

newtype DNSR = DNSR Resolver
             deriving (Typeable)

dnsResolver :: Variable
dnsResolver = mkVar "dnsresolver"

getDNSResolver :: Smtpd Resolver
getDNSResolver = do
  DNSR f <- local (getval dnsResolver)
        >>= maybe (global $ getval_ dnsResolver) return
  return f

queryA :: HostName -> Smtpd (Maybe [HostAddress])
queryA h = getDNSResolver >>= \r -> liftIO $ query resolveA r h

queryPTR :: HostAddress -> Smtpd (Maybe [HostName])
queryPTR h = getDNSResolver >>= \r -> liftIO $ query resolvePTR r h

queryMX :: HostName -> Smtpd (Maybe [(HostName, HostAddress)])
queryMX h = getDNSResolver >>= \r -> liftIO $ query resolveMX r h
