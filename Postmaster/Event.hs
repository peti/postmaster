{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.Event
   Copyright   :  (c) 2005-02-09 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.Event where

import Foreign
import Control.Exception
import Control.Monad.RWS hiding ( local )
import System.Directory
import System.IO
import Network ( HostName )
import Postmaster.Base
import Postmaster.IO
import MonadEnv
import Rfc2821 hiding ( path )
import BlockIO
import Digest

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
  . initHeloName heloName
  . setMailID
  . setMailFrom
  . setIsEhloPeer
  . setPeerHelo
  . feedPayload "/tmp/test-spool"
  $ event

-- ** Local Variable: @SessionState@

setSessionState :: SessionState -> Smtpd ()
setSessionState = local . setval (mkVar "SessionState")

-- |Will 'fail' when @SessionState@ is not set.

getSessionState :: Smtpd SessionState
getSessionState = local $ getDefault (mkVar "SessionState") Unknown

-- ** Local Variable: @HeloName@

-- |Initialized during the 'Greeting' event; will not
-- overwrite the variable if it does exist already.

initHeloName :: HostName -> EventT
initHeloName n f e = do
  when (e == Greeting) (local $ withval_ (mkVar "HeloName") (maybe n id))
  f e

-- |Will 'fail' when @HeloName@ is not set.

myHeloName :: Smtpd HostName
myHeloName = local $ getval_ (mkVar "HeloName")


-- ** Local Variable: @IsEhloPeer@

setIsEhloPeer :: EventT
setIsEhloPeer f e = do
  r@(Reply rc _) <- f e
  let set = local . setval (mkVar "IsEhloPeer")
  case (e,rc) of
    (SayEhlo _, Code Success _ _) -> set True
    (SayHelo _, Code Success _ _) -> set False
    (_, _)                        -> return ()
  return r

isEhloPeer :: Smtpd Bool
isEhloPeer = local $ getDefault (mkVar "IsEhloPeer") False

-- ** Local Variable: @PeerHelo@

-- |Set when 'SayHelo' or 'SayEhlo' succeed.

setPeerHelo :: EventT
setPeerHelo f e = do
  r@(Reply rc _) <- f e
  let set = local . setval (mkVar "PeerHelo")
  case (e,rc) of
    (SayEhlo peer, Code Success _ _) -> set peer
    (SayHelo peer, Code Success _ _) -> set peer
    (_, _)                           -> return ()
  return r

-- |Will 'fail' when @PeerHelo@ is not set.

getPeerHelo :: Smtpd HostName
getPeerHelo = local $ getval_ (mkVar "PeerHelo")


-- ** Local Variable: @MailFrom@

-- |Set when 'SetMailFrom' event succeeds, unset during
-- 'ResetState'.

setMailFrom :: EventT
setMailFrom f e = do
  r@(Reply rc _) <- f e
  case (e,rc) of
    (SetMailFrom x, Code Success _ _) -> local $ setval (mkVar "MailFrom") x
    (ResetState, _)                   -> local $ unsetval (mkVar "MailFrom")
    (_, _)                            -> return ()
  return r

getMailFrom :: Smtpd Mailbox
getMailFrom = local $ getval_ (mkVar "MailFrom")


-- -- ** Local Variable: @RcptTo@
--
-- setRcptTo :: [Target] -> Smtpd ()
-- setRcptTo = local . setval (mkVar "RcptTo")
--
-- getRcptTo :: Smtpd [Target]
-- getRcptTo = local $ getDefault (mkVar "RcptTo") []
--
-- addRcptTo :: Target -> Smtpd ()
-- addRcptTo m = getRcptTo >>= setRcptTo . (m:)


-- ** Local Variable: @MailID@

-- |Set when 'SetMailFrom' succeeds; unset during
-- 'ResetState'.

setMailID :: EventT
setMailID f e = do
  r@(Reply rc _) <- f e
  case (e,rc) of
    (SetMailFrom _, Code Success _ _) -> getUniqueID >>= local . setval (mkVar "MailID")
    (ResetState, _)                   -> local $ unsetval (mkVar "MailID")
    (_, _)                            -> return ()
  return r

-- |Will 'fail' when @MailID@ is not set.

getMailID :: Smtpd ID
getMailID = local $ getval_ (mkVar "MailID")


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

event StartData = event NotImplemened

event Deliver   = event NotImplemened


----------------------------------------------------------------------
-- * The Standard Bad-Ass Payload Handler
----------------------------------------------------------------------

feedPayload :: FilePath -> EventT

feedPayload spool _ StartData =
  do mkSHA1 >>= local . setval (mkVar "sha1engine")
     mid <- getMailID
     let path = spool ++ "/temp." ++ show mid
     local $ setval (mkVar "spoolname") path
     h <- liftIO (openBinaryFile path WriteMode)
     local $ setval (mkVar "spoolhandle") h
     setDataHandler (dataHandler h)
     say 3 5 4 "terminate data with <CRLF>.<CRLF>"
  `fallback`
    say 4 5 1 "requested action aborted: error in processing"

feedPayload spool _ Deliver = do
  do let skey = mkVar "sha1engine"
         hkey = mkVar "spoolhandle"
         fkey = mkVar "spoolname"
     local (getval_ hkey) >>= liftIO . hClose >> local (unsetval hkey)
     ctx   <- local (getval_ skey)
     fname <- local (getval_ fkey)
     sha1  <- liftIO $ do
        sha1 <- (evalStateT final ctx >>= return . (>>= toHex))
                   `finally` (\(DST x) -> ctxDestroy x) ctx
        let fname' = spool ++ "/" ++ sha1
        renameFile fname fname'
        return sha1
     local (unsetval skey >> unsetval fkey)
     say 2 5 0 (sha1 ++ " message accepted for delivery")
  `fallback`
    say 4 5 1 "requested action aborted: error in processing"

feedPayload _ f ResetState = cleanupSpool >> f ResetState
feedPayload _ f Shutdown   = cleanupSpool >> f Shutdown
feedPayload _ f e = f e

-- ** Data Handler Helpers

dataHandler :: WriteHandle -> DataHandler
dataHandler _ buf@(Buf _ _ 0) = return (Nothing, buf)
dataHandler hOut buf@(Buf _ ptr n) = do
  xs <- liftIO (peekArray (fromIntegral n) ptr)
  let theEnd   = map (toEnum . fromEnum) "\r\n.\r\n"
      (eod, i) = case strstr theEnd xs of
                   Nothing -> (False, max 0 (n - 4))
                   Just j -> (True, fromIntegral (j-3))
      i'       = fromIntegral i
  liftIO (hPutBuf hOut ptr i')
  local (getval_ (mkVar "sha1engine"))
    >>= liftIO . execStateT (update' (ptr, i'))
    >>= local . setval (mkVar "sha1engine")
  buf' <- liftIO (flush i buf)
  if not eod then return (Nothing, buf') else do
    r <- trigger Deliver
    trigger ResetState
    setSessionState HaveHelo
    return (Just r, buf')

mkSHA1 :: Smtpd DigestState
mkSHA1 = liftIO $
  bracketOnError ctxCreate ctxDestroy $ \ctx -> do
    when (ctx == nullPtr) (fail "can't initialize SHA1 digest context")
    md <- toMDEngine SHA1
    when (md == nullPtr) (fail "can't initialize SHA1 digest engine")
    rc <- digestInit ctx md
    when (rc == 0) (fail "can't initialize SHA1 digest")
    return (DST ctx)

cleanupSpool :: Smtpd ()
cleanupSpool = do
  let sha1 = mkVar "sha1engine"
      hkey = mkVar "spoolhandle"
      fkey = mkVar "spoolname"
  local (getval sha1)
    >>= maybe (return ()) (\(DST ctx) -> liftIO (ctxDestroy ctx))
  local (getval hkey) >>= maybe (return ()) (liftIO . hClose)
  local (getval fkey) >>= maybe (return ()) (liftIO . removeFile)
  local (unsetval sha1 >> unsetval hkey >> unsetval fkey)
