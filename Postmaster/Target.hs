{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.Target
   Copyright   :  (c) 2005-02-06 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.Target where

import Prelude hiding ( catch )
import Foreign
import Control.Concurrent
import Control.Exception
import Control.Monad.RWS hiding ( local )
import System.Exit ( ExitCode(..) )
import System.IO
import System.IO
import System.Process
import Data.Typeable
import Data.List ( nub )
import Postmaster.Base
import Postmaster.Event
import Rfc2821 hiding ( path )
import Child ( timeout )
import MonadEnv
-- import BlockIO

-- * Mail Targets

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
-- this just causes execution of @\/usr\/sbin\/sendmail@
-- with appropriate flags.

relay :: [Mailbox] -> Smtpd SmtpReply
relay rs = do
  addRcptTo $ Target rs Relay Ready
  say 2 5 0 "recipient ok"

-- * Generic Targets

data Target = Target [Mailbox] MailHandler MailerStatus
            deriving (Typeable)

data MailHandler
  = Pipe FilePath [String]
  | Relay

data MailerStatus
  = Ready
  | Live ExternHandle
  | Failed
  | FailedPermanently
  | Succeeded

-- ** Local Variable: @RcptTo@

setRcptTo :: [Target] -> Smtpd ()
setRcptTo = local . setval (mkVar "RcptTo")

getRcptTo :: Smtpd [Target]
getRcptTo = local $ getDefault (mkVar "RcptTo") []

addRcptTo :: Target -> Smtpd ()
addRcptTo m = getRcptTo >>= setRcptTo . (m:)

-- ** TODO: All crap! Has to go away.

type ExternHandle = MVar (Handle, Handle, Handle, ProcessHandle)

-- |Run an external process and store its handle in an
-- 'MVar' with a finalizer attached to it that will close
-- the handles and kill the process when the MVar falls out
-- of scope. The process is run with \"@\/@\" as current
-- directory and an /empty/ enviroment. (This may change.)

extern :: FilePath -> [String] -> IO ExternHandle
extern path args = do
  r <- runInteractiveProcess path args (Just "/") (Just [])
  mv <- newMVar r
  addMVarFinalizer mv (catch (cleanup r) (const (return ())))
  let (hin,_,_,_) = r
  hSetBuffering hin NoBuffering
  return mv
    where
    cleanup (hin, hout, herr, pid) = do
      terminateProcess pid
      hClose hin >> hClose hout >> hClose herr
      safeWaitForProcess pid
      return ()

-- |Wait 30 seconds max. If the process hasn't terminated by
-- then, throw an exception. If the child process has been
-- terminated by a signal, return @ExitFailure 137@. This is
-- a kludge. So it will probably be in here forever.

safeWaitForProcess :: ProcessHandle -> IO ExitCode
safeWaitForProcess pid =
  timeout maxwait loop >>= maybe badluck return
    where
    loop    = catch loop' (\_ -> return (ExitFailure 137))
    loop'   = wait >> getProcessExitCode pid >>= maybe loop' return
    wait    = threadDelay 1000000 -- 1 second
    maxwait = 30000000            -- 30 seconds
    badluck = fail "timeout while waiting for external process"


----------------------------------------------------------------------
-- * Standard Data Handler
----------------------------------------------------------------------

-- type DataHandler = Buffer -> Smtpd (Maybe SmtpReply, Buffer)
--
-- newtype DH = DH DataHandler   deriving (Typeable)
--
-- -- ** |Local Variable: @DataHandler@
--
-- setDataHandler :: DataHandler -> Smtpd ()
-- setDataHandler f = local (setval key (DH f))
--   where key = mkVar "DataHandler"
--
-- myDataHandler :: Smtpd DataHandler
-- myDataHandler = local (getval_ key) >>= \(DH f) -> return f
--   where key = mkVar "DataHandler"
--
-- feed :: DataHandler
-- feed buf = myDataHandler >>= ($ buf)

--  (Buf _ ptr n) = undefined

-- handleData :: Buffer -> Smtpd (Maybe SmtpReply, Buffer)
-- handleData buf@(Buf _ ptr n) = do
--   xs <- liftIO (peekArray (fromIntegral n) ptr)
--   let eod = map (toEnum . fromEnum) "\r\n.\r\n"
--   case strstr eod xs of
--     Nothing -> do let n' = max 0 (n - 4)
--                   feed (ptr, fromIntegral n')
--                   buf' <- liftIO $ flush n' buf
--                   return (Nothing, buf')
--     Just i  -> do feed (ptr, (i-3))
--                   r <- trigger Deliver
--                   trigger ResetState
--                   setSessionState HaveHelo
--                   buf' <- liftIO $ flush (fromIntegral i) buf
--                   return (Just r, buf')
--

-- feed ( _ , 0) = return ()
-- feed (ptr, n) = getRcptTo >>= mapM (feedTarget (ptr,n)) >>= setRcptTo



feedPayload :: EventT
feedPayload _ StartData = do
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

feedPayload _ Deliver = do
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

feedPayload f e = f e

-- |Make a 'Ready' target 'Live'.

startTarget :: Target -> Smtpd Target

startTarget (Target rs mh@(Pipe path args) Ready) = do
--  yell (StartExternal rs (path:args))
  mv <- liftIO (extern path args)
  return (Target rs mh (Live mv))

startTarget (Target rs Relay Ready) = do
  from <- getMailFrom
  let mta = "/usr/sbin/sendmail"  -- TODO
  let flags = [ "-f" ++ show from ] ++ map show rs
      t'    = Target rs (Pipe mta flags) Ready
  Target _ _ mst <- startTarget t'
  return (Target rs Relay mst)

startTarget t = {- yell (UnknownStartTarget t) >> -} return t

-- |Give a target a chunk of data.

feedTarget :: (Ptr Word8, Int) -> Target -> Smtpd Target
feedTarget (ptr,n) t@(Target _ _ (Live mv)) = do
  liftIO (withMVar mv (\(hin,_,_,_) -> hPutBuf hin ptr n))
  return t

feedTarget _ t = {- yell (UnknownFeedTarget t) >> -} return t

-- |Close a target.

closeTarget :: Target -> Smtpd Target
closeTarget t@(Target _ _ (Live mv)) = do
  liftIO (withMVar mv (\(hin,_,_,_) -> hClose hin))
  return t

closeTarget t = return t

-- |Update a targets 'MailerStatus' to signify success or
-- failure.

commitTarget :: Target -> Smtpd Target
commitTarget (Target rs mh (Live mv)) = do
  rc <- liftIO $ do
    (hin,hout,herr,pid) <- takeMVar mv
    catch (hClose hin) (const (return ()))
    safeWaitForProcess pid
      `finally` hClose hout
      `finally` hClose herr
  -- yell (ExternalResult t rc)
  if rc == ExitSuccess
     then return (Target rs mh Succeeded)
     else if    (rc == ExitFailure 65)   -- EX_DATAERR
             || (rc == ExitFailure 67)   -- EX_NOUSER
             || (rc == ExitFailure 68)   -- EX_NOHOST
             then return (Target rs mh FailedPermanently)
             else return (Target rs mh Failed)

commitTarget t = {- yell (UnknownCommitTarget t) >> -} return t
