{- |
   Module      :  Postmaster.Event
   Copyright   :  (c) 2005-02-03 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre

   The Postmaster ESMTP Server. See
   <http://postmaster.cryp.to/docs/tutorial.html> for an
   introduction. Note that you should /not/ modify this
   module. In theory, you should be able to achieve any
   effect you like through modifying the contents of
   'Config'. If there is something that appears to be
   impossible to do without modifying the core modules,
   please complain loudly to <mailto:postmaster-dev@lists.cryp.to>.
 -}

module Postmaster.Event where

import Control.Monad.RWS hiding ( local )
import Data.List ( nub )
import Data.Unique
import Rfc2821 hiding ( path )
import Postmaster.Base

event :: Event -> Smtpd SmtpReply

event Greeting = do
  whoami <- asks myHeloName
  say 2 2 0 (showString whoami " Postmaster ESMTP Server")

event Shutdown = do
  whoami <- asks myHeloName
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

event (ResetState) = do
  modify (\st -> st { mailFrom     = mailFrom initSmtpd
                    , rcptTo       = rcptTo initSmtpd
                    , mailID       = mailID initSmtpd
                    })
  say 2 5 0 "state reset"

event (SeeksHelp []) =
  say 5 0 2 "Why don't you ask about something specific?"

event (SeeksHelp _) =
  say 5 0 4 "I don't implement HELP with parameters."

event (SayHelo peer) = do
  trigger eventHandler ResetState
  modify (\st -> st { peerHelo = peer, isEhloPeer = False })
  whoami <- asks myHeloName
  say 2 5 0 (showString whoami " Postmaster; pleased to meet you.")

event (SayHeloAgain peer) = do
  whoami <- asks myHeloName
  peer' <- gets peerHelo
  r@(Reply c@(Code suc _ _) _) <- trigger eventHandler (SayHelo peer)
  if (peer /= peer' || suc /= Success)
     then return r
     else return (Reply c [msg whoami])
  where
  msg w = showString w " Yeah, yeah. You said that before."

event (SayEhlo peer) = do
  r <- trigger eventHandler (SayHelo peer)
  case r of
    Reply c@(Code Success _ _) (x:_) -> do
      modify (\st -> st { isEhloPeer = True })
      return (Reply c (x:capabilities))
    Reply _ _   -> return r
  where
  capabilities = [ "PIPELINING" ]

event (SayEhloAgain peer) = do
  whoami <- asks myHeloName
  peer' <- gets peerHelo
  r@(Reply c@(Code suc _ _) (_:xs)) <- trigger eventHandler (SayEhlo peer)
  if (peer /= peer' || suc /= Success)
     then return r
     else return (Reply c ((msg whoami):xs))
  where
  msg w = showString w " Sorry, I wasn't listening for a moment."

event (SetMailFrom mbox) = do
  trigger eventHandler ResetState
  mid <- liftIO (fmap hashUnique newUnique)
  yell (AssignMailID mid)
  modify (\st -> st { mailFrom = mbox
                    , mailID   = mid
                    })
  say 2 5 0 (mbox `shows` " ... sender ok")

event (AddRcptTo mbox) =
  say 5 5 3 (mbox `shows` " ... unknown recipient")

event (StartData) = do
  ts <- gets rcptTo
  let isrelay (Target _ Relay Ready) = True
      isrelay _                      = False
      tlocal  = filter (not . isrelay) ts
      batch   = [ rs | Target rs Relay Ready <- ts ]
      relayt  = case nub (concat batch) of
                  [] -> []
                  rs -> [Target rs Relay Ready]
  ts' <- mapM startTarget (tlocal ++ relayt)
  modify (\st -> st { rcptTo = ts' })
  say 3 5 4 "terminate data with <CRLF>.<CRLF>"

event Deliver = do
  ts <- gets rcptTo >>= mapM closeTarget >>= mapM commitTarget
  modify (\st -> st { rcptTo = ts })
  let isSuccess  (Target _ _ Succeeded) = True
      isSuccess  _                      = False
      oneOK = any isSuccess ts
  let isPermFail (Target _ _ FailedPermanently) = True
      isPermFail _                              = False
      allPermFail = all isPermFail ts
  case (oneOK, allPermFail) of
    (True ,   _  ) -> do mid <- gets mailID
                         say 2 5 0 (mid `shows` " message accepted for delivery")
    (False, False) -> say 4 5 1 "requested action aborted: error in processing"
    (  _  , True ) -> say 5 5 4 "transaction failed"


-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-ghci-program-args: ( "-ladns" "-lcrypto" ) ***
-- End: ***
