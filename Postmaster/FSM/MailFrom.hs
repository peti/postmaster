{- |
   Module      :  Postmaster.FSM.MailFrom
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.MailFrom where

import Postmaster.Base
import MonadEnv
import Text.ParserCombinators.Parsec.Rfc2821

-- |Local Variable: @MAILFROM :: 'Mailbox'@

mailFrom :: SmtpdVariable
mailFrom = defineLocal "mailfrom"

handleMailFrom :: EventT
handleMailFrom f e = do
  r <- f e
  case (e, isSuccess r) of
    (SetMailFrom x, True) -> mailFrom (`setval` x)
    (ResetState, _)       -> mailFrom unsetval
    (_, _)                -> return ()
  return r

getMailFrom :: Smtpd Mailbox
getMailFrom = mailFrom getval_
