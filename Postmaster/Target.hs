{- |
   Module      :  Postmaster.Target
   Copyright   :  (c) 2005-02-05 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.Target where

import System.IO
import Control.Monad.RWS hiding ( local )
import Rfc2821 hiding ( path )
import Postmaster.Base
import Postmaster.Event

-- |Create 'Pipe' target and 'addRcptTo' it. The mailbox
-- parameter is just annotation; but it's a good idea to use
-- it to associate the original e-mail address with the
-- target. If nothing else, it makes the log messages more
-- informative. Note that Postmaster pipes the data section
-- into the program /unmodified/. CRLF line-endings, escaped
-- dot-lines, and all that.

pipe :: [Mailbox] -> FilePath -> [String] -> Smtpd SmtpReply
pipe _   []   _   = fail "Postmaster.pipe: path may not be empty"
pipe rs path args = do
  addRcptTo $ Target rs (Pipe path args) Ready
  say 2 5 0 "recipient ok"

-- |Wrapper for 'pipe' which runs the command with
-- @\/bin\/sh -c@ so that you can use in-\/output
-- redirection, piping, etc. This wrapper will also convert
-- the data section to standard text. Meaning that
-- \"@\\r\\n@\" is converted to \"@\\n@\" and
-- \"@\\r\\n..\\r\\n@\" is unquoted to \"@\\r\\n.\\r\\n@\".
-- Procmail needs that, for example.

shell :: [Mailbox] -> String -> Smtpd SmtpReply
shell   _   [] = fail "Postmaster.shell: command may not be empty"
shell mbox cmd = pipe mbox "/bin/sh" [ "-c", cmd']
  where
  cmd'   = toText ++ " | " ++ cmd
  toText = "sed -e 's/\r$//' -e 's/^\\.\\.$/./'"

-- |Create a 'Relay' target and 'addRcptTo' it. Currently,
-- this just causes execution of 'sendmailPath'. with
-- appropriate flags.

relay :: [Mailbox] -> Smtpd SmtpReply
relay rs = do
  addRcptTo $ Target rs Relay Ready
  say 2 5 0 "recipient ok"


-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-ghci-program-args: ( "-ladns" "-lcrypto" ) ***
-- End: ***
