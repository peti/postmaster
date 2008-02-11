{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.FSM.DNSResolver
   Copyright   :  (c) 2004-2008 by Peter Simons
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
import Control.Monad.Trans

newtype DNSR = DNSR Resolver
             deriving (Typeable)

dnsResolver :: Variable
dnsResolver = mkVar "dnsresolver"

setDNSResolver :: Resolver -> EnvT ()
setDNSResolver f = setVar dnsResolver (DNSR f)

getDNSResolver :: Smtpd Resolver
getDNSResolver = do
  DNSR f <- local (getVar dnsResolver)
        >>= maybe (global $ getVar_ dnsResolver) return
  return f

queryA :: HostName -> Smtpd (Maybe [HostAddress])
queryA h = getDNSResolver >>= \r -> liftIO $ query resolveA r h

queryPTR :: HostAddress -> Smtpd (Maybe [HostName])
queryPTR h = getDNSResolver >>= \r -> liftIO $ query resolvePTR r h

queryMX :: HostName -> Smtpd (Maybe [(HostName, HostAddress)])
queryMX h = getDNSResolver >>= \r -> liftIO $ query resolveMX r h
