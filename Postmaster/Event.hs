{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.Event
   Copyright   :  (c) 2005-02-05 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.Event where

import Prelude hiding ( catch )
import Control.Monad.RWS hiding ( local )
import Data.List ( nub )
-- import Data.Typeable
import Control.Exception
import Control.Concurrent
import Foreign
import System.IO
import Network ( HostName )
import System.Exit ( ExitCode(..) )
import Postmaster.Base
import Postmaster.Extern
import Rfc2821 hiding ( path )
import MonadEnv

----------------------------------------------------------------------
-- * Event Handlers
----------------------------------------------------------------------

-- |We write combinators that transform an event handler to
-- add functionality. Event handlers usually update and
-- provide variables in the local environment. You'll
-- discover a rather obvious naming convention in this case.

type EventT = (Event -> Smtpd SmtpReply)
            -> Event -> Smtpd SmtpReply

-- |Generate the standard ESMTP event handler.

mkEvent :: HostName -> Event -> Smtpd SmtpReply
mkEvent heloName
  = announce "PIPELINING"
  . setHeloName heloName
  . setSessionID
  . setMailID
  . setMailFrom
  . setIsEhloPeer
  . setPeerHelo
  $ event


-- ** Local Variable: @SessionState@

setSessionState :: SessionState -> Smtpd ()
setSessionState = local . setval "SessionState"

-- |Will 'fail' when @SessionState@ is not set.

getSessionState :: Smtpd SessionState
getSessionState = local $ getDefault "SessionState" Unknown

-- ** Local Variable: @HeloName@

-- |Set during the 'Greeting' event.

setHeloName :: HostName -> EventT
setHeloName n f e = do
  when (e == Greeting) (local (setval "HeloName" n))
  f e

-- |Will 'fail' when @HeloName@ is not set.

myHeloName :: Smtpd HostName
myHeloName = local $ getval_ "HeloName"


-- ** Local Variable: @IsEhloPeer@

setIsEhloPeer :: EventT
setIsEhloPeer f e = do
  r@(Reply rc _) <- f e
  let set = local . setval "IsEhloPeer"
  case (e,rc) of
    (SayEhlo _, Code Success _ _) -> set True
    (SayHelo _, Code Success _ _) -> set False
    (_, _)                        -> return ()
  return r

isEhloPeer :: Smtpd Bool
isEhloPeer = local $ getDefault "IsEhloPeer" False

-- ** Local Variable: @PeerHelo@

-- |Set when 'SayHelo' or 'SayEhlo' succeed.

setPeerHelo :: EventT
setPeerHelo f e = do
  r@(Reply rc _) <- f e
  let set = local . setval "PeerHelo"
  case (e,rc) of
    (SayEhlo peer, Code Success _ _) -> set peer
    (SayHelo peer, Code Success _ _) -> set peer
    (_, _)                           -> return ()
  return r

-- |Will 'fail' when @PeerHelo@ is not set.

getPeerHelo :: Smtpd HostName
getPeerHelo = local $ getval_ "PeerHelo"


-- ** Local Variable: @MailFrom@

-- |Set when 'SetMailFrom' event succeeds, unset during
-- 'ResetState'.

setMailFrom :: EventT
setMailFrom f e = do
  r@(Reply rc _) <- f e
  case (e,rc) of
    (SetMailFrom x, Code Success _ _) -> local $ setval "MailFrom" x
    (ResetState, _)                   -> local $ unsetval "MailFrom"
    (_, _)                            -> return ()
  return r

getMailFrom :: Smtpd Mailbox
getMailFrom = local $ getval_ "MailFrom"


-- ** Local Variable: @RcptTo@

setRcptTo :: [Target] -> Smtpd ()
setRcptTo = local . setval "RcptTo"

getRcptTo :: Smtpd [Target]
getRcptTo = local $ getDefault "RcptTo" []

addRcptTo :: Target -> Smtpd ()
addRcptTo m = getRcptTo >>= setRcptTo . (m:)


-- ** Unique Identifier Generation

-- |Produce a unique 'ID' using a global counter.

getUniqueID :: Smtpd ID
getUniqueID = global $ tick "UniqueID"

-- *** Local Variable: @SessionID@

-- |Provides a unique @SessionID@ variable during the
-- 'Greeting' event

setSessionID :: EventT
setSessionID f e = do
  when (e == Greeting)
       (getUniqueID >>= local . setval "SessionID")
  f e

-- |Will 'fail' when @SessionID@ is not set.

mySessionID :: Smtpd ID
mySessionID = local $ getval_ "SessionID"


-- *** Local Variable: @MailID@

-- |Set when 'SetMailFrom' succeeds; unset during
-- 'ResetState'.

setMailID :: EventT
setMailID f e = do
  r@(Reply rc _) <- f e
  case (e,rc) of
    (SetMailFrom _, Code Success _ _) -> getUniqueID >>= local . setval "MailID"
    (ResetState, _)                   -> local $ unsetval "MailID"
    (_, _)                            -> return ()
  return r

-- |Will 'fail' when @MailID@ is not set.

getMailID :: Smtpd ID
getMailID = local $ getval_ "MailID"

-- ** Combinator: Announce ESMTP Capability

-- |Append the given ESMTP keyword to the reply produced
-- during 'SayEhlo'.

announce :: String -> EventT
announce keyword f e@(SayEhlo _) = do
  Reply rc msg <- f e
  let msg' = msg ++ [keyword]
  case rc of
    Code Success _ _ -> return (Reply rc msg')
    _                -> return (Reply rc msg)
announce _ f e = f e


----------------------------------------------------------------------
-- * The Standard Bad-Ass Event Handler
----------------------------------------------------------------------

event :: Event -> Smtpd SmtpReply

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

event (StartData) = do
  ts <- getRcptTo
  let isrelay (Target _ Relay Ready) = True
      isrelay _                      = False
      tlocal  = filter (not . isrelay) ts
      batch   = [ rs | Target rs Relay Ready <- ts ]
      relayt  = case nub (concat batch) of
                  [] -> []
                  rs -> [Target rs Relay Ready]
  mapM startTarget (tlocal ++ relayt) >>= setRcptTo
  say 3 5 4 "terminate data with <CRLF>.<CRLF>"

event Deliver = do
  ts <- getRcptTo >>= mapM closeTarget >>= mapM commitTarget
  setRcptTo ts
  let isSuccess  (Target _ _ Succeeded) = True
      isSuccess  _                      = False
      oneOK = any isSuccess ts
  let isPermFail (Target _ _ FailedPermanently) = True
      isPermFail _                              = False
      allPermFail = all isPermFail ts
  case (oneOK, allPermFail) of
    (True ,   _  ) -> do mid <- getMailID
                         say 2 5 0 (mid `shows` " message accepted for delivery")
    (False, False) -> say 4 5 1 "requested action aborted: error in processing"
    (  _  , True ) -> say 5 5 4 "transaction failed"


----------------------------------------------------------------------
-- * Standard Data Handler
----------------------------------------------------------------------

feed :: (Ptr Word8, Int) -> Smtpd ()
feed ( _ , 0) = return ()
feed (ptr, n) = getRcptTo >>= mapM (feedTarget (ptr,n)) >>= setRcptTo

-- |Make a 'Ready' target 'Live'.

startTarget :: Target -> Smtpd Target

startTarget (Target rs mh@(Pipe path args) Ready) = do
  yell (StartExternal rs (path:args))
  mv <- liftIO (extern path args)
  return (Target rs mh (Live mv))

startTarget (Target rs Relay Ready) = do
  from <- getMailFrom
  let mta = "/usr/sbin/sendmail"  -- TODO
  let flags = [ "-f" ++ show from ] ++ map show rs
      t'    = Target rs (Pipe mta flags) Ready
  Target _ _ mst <- startTarget t'
  return (Target rs Relay mst)

startTarget t = yell (UnknownStartTarget t) >> return t

-- |Give a target a chunk of data.

feedTarget :: (Ptr Word8, Int) -> Target -> Smtpd Target
feedTarget (ptr,n) t@(Target _ _ (Live mv)) = do
  liftIO (withMVar mv (\(hin,_,_,_) -> hPutBuf hin ptr n))
  return t

feedTarget _ t = yell (UnknownFeedTarget t) >> return t

-- |Close a target.

closeTarget :: Target -> Smtpd Target
closeTarget t@(Target _ _ (Live mv)) = do
  liftIO (withMVar mv (\(hin,_,_,_) -> hClose hin))
  return t

closeTarget t = return t

-- |Update a targets 'MailerStatus' to signify success or
-- failure.

commitTarget :: Target -> Smtpd Target
commitTarget t@(Target rs mh (Live mv)) = do
  rc <- liftIO $ do
    (hin,hout,herr,pid) <- takeMVar mv
    catch (hClose hin) (const (return ()))
    safeWaitForProcess pid
      `finally` hClose hout
      `finally` hClose herr
  yell (ExternalResult t rc)
  if rc == ExitSuccess
     then return (Target rs mh Succeeded)
     else if    (rc == ExitFailure 65)   -- EX_DATAERR
             || (rc == ExitFailure 67)   -- EX_NOUSER
             || (rc == ExitFailure 68)   -- EX_NOHOST
             then return (Target rs mh FailedPermanently)
             else return (Target rs mh Failed)

commitTarget t = yell (UnknownCommitTarget t) >> return t


-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-ghci-program-args: ( "-ladns" "-lcrypto" ) ***
-- End: ***
