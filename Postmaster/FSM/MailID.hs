{- |
   Module      :  Postmaster.FSM.MailID
   Copyright   :  (c) 2005-02-13 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.MailID where

import Postmaster.Base
import Control.Monad.Env
import Text.ParserCombinators.Parsec.Rfc2821

-- |Local Variable: @MAILID :: 'ID'@

mailID :: SmtpdVariable
mailID = defineLocal "mailid"

-- |Set when 'SetMailFrom' succeeds; unset during
-- 'ResetState'.

handleMailID :: EventT
handleMailID f e = do
  r <- f e
  case (e, isSuccess r) of
    (SetMailFrom _, True) -> getUniqueID >>= \x -> mailID (`setVar` x)
    (ResetState   ,  _  ) -> mailID unsetVar
    (_, _)                -> return ()
  return r

-- |Will 'fail' when @MailID@ is not set.

getMailID :: Smtpd ID
getMailID = mailID getVar_
