{- |
   Module      :  Postmaster.FSM
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM
  ( mkEvent
  , announce
  , queryA, queryPTR, queryMX
  , feed
  , isEhloPeer
  , trigger
  , myHeloName
  , getMailFrom
  , getMailID
  , getPeerAddr
  , getPeerHelo
  , getSessionState
  )
  where

import Network ( HostName )
import Postmaster.Base
import Postmaster.FSM.Announce
import Postmaster.FSM.DNSResolver
import Postmaster.FSM.DataHandler
import Postmaster.FSM.EhloPeer
import Postmaster.FSM.EventHandler
import Postmaster.FSM.HeloName
import Postmaster.FSM.MailFrom
import Postmaster.FSM.MailID
import Postmaster.FSM.PeerAddr
import Postmaster.FSM.PeerHelo
import Postmaster.FSM.SessionState
import Postmaster.FSM.Spooler
import Rfc2821

-- |Generate the standard ESMTP event handler. The
-- parameters are the path to the spool directory and our
-- @HELO@ name.

mkEvent :: HostName -> FilePath -> EventHandler
mkEvent heloname spooldir
  = announce "PIPELINING"
  . initHeloName heloname
  . handleMailID
  . handleMailFrom
  . handleEhloPeer
  . handlePeerHelo
  . handlePayload spooldir
  $ event

-- |The Standard Bad-Ass Event Handler

event :: EventHandler

event Greeting = do
  whoami <- myHeloName
  say 2 2 0 (showString whoami " Postmaster ESMTP Server")

event Shutdown = do
  whoami <- myHeloName
  say 2 2 1 (showString whoami " closing connection")

event NotImplemened =
  say 5 0 2 "command not implemented"

event (Unrecognized _) =
  say 5 0 0 "unrecognized command"

event (SyntaxErrorIn cmd) =
  say 5 0 1 (showString "syntax error in parameters or arguments of " cmd)

event (SayOK) =
  say 2 5 0 "Massive system failure. Just kidding ... ok."

event (NeedHeloFirst) =
  say 5 0 3 "You should say HELO first."

event (NeedMailFromFirst) =
  say 5 0 3 "What MAIL are you talking about?"

event (NeedRcptToFirst) =
  say 5 0 3 "Care to tell me where I should send it to?"

event (ResetState) =
  say 2 5 0 "state reset"

event (SeeksHelp []) =
  say 5 0 2 "Why don't you ask about something specific?"

event (SeeksHelp _) =
  say 5 0 4 "I don't implement HELP with parameters."

event (SayHelo _) = do
  trigger ResetState
  whoami <- myHeloName
  say 2 5 0 (showString whoami " Postmaster; pleased to meet you.")

event (SayEhlo peer)      = event (SayHelo peer)
event (SayHeloAgain peer) = event (SayHelo peer)
event (SayEhloAgain peer) = event (SayHelo peer)

event (SetMailFrom mbox) = do
  trigger ResetState
  say 2 5 0 (mbox `shows` " ... sender ok")

event (AddRcptTo mbox) =
  say 5 5 3 (mbox `shows` " ... unknown recipient")

event StartData = event NotImplemened

event Deliver   = event NotImplemened
