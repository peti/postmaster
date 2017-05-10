{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
{- |
   Module      :  Postmaster.FSM.MailFrom
   Copyright   :  (c) 2004-2008 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.MailFrom where

import Postmaster.Base
import Text.Parsec.Rfc2821
import Data.Typeable

newtype SmtpMailbox = SMB Mailbox
                      deriving (Typeable)

-- |Local Variable: @MAILFROM :: 'Mailbox'@

mailFrom :: SmtpdVariable
mailFrom = defineLocal "mailfrom"

handleMailFrom :: EventT
handleMailFrom f e = do
  r <- f e
  case (e, isSuccess r) of
    (SetMailFrom x, True) -> mailFrom (`setVar` (SMB x))
    (ResetState, _)       -> mailFrom unsetVar
    (_, _)                -> return ()
  return r

getMailFrom :: Smtpd Mailbox
getMailFrom = mailFrom getVar_ >>= \(SMB x) -> return x
