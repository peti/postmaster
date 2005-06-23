{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.FSM.Spooler
   Copyright   :  (c) 2005-02-13 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.Spooler where

import Foreign
import Control.Exception
import Control.Concurrent.MVar
import Control.Monad.RWS hiding ( local )
import System.Directory
import System.IO
import Postmaster.Base
import Postmaster.FSM.EventHandler
import Postmaster.FSM.SessionState
import Postmaster.FSM.DataHandler
import Postmaster.FSM.MailID
import Control.Monad.Env
import Text.ParserCombinators.Parsec.Rfc2821 hiding ( path )
import System.IO.Driver
import OpenSSL.Digest
import Data.Typeable

data Spooler = S (Maybe FilePath) (Maybe WriteHandle) DigestState
             deriving (Typeable)

spoolerState :: SmtpdVariable
spoolerState = defineLocal "spoolerstate"

getState :: Smtpd (MVar Spooler)
getState = spoolerState getVar_

setState :: MVar Spooler -> Smtpd ()
setState st = spoolerState (`setVar` st)

-- |The Standard Bad-Ass Payload Handler. Needs the path to
-- the spool directory.

handlePayload :: FilePath -> EventT

handlePayload _ f Greeting = do
  liftIO (newMVar (S Nothing Nothing (DST nullPtr))) >>= setState
  setDataHandler feeder
  f Greeting

handlePayload spool _ StartData =
  do st <- getState
     mid <- getMailID
     let path = spool ++ "/temp." ++ show mid
     liftIO . modifyMVar_ st $ \(S p' h' (DST c')) ->
       assert (p' == Nothing) $
       assert (h' == Nothing) $
       assert (c' == nullPtr) $
       Postmaster.Base.bracketOnError
         (openBinaryFile path WriteMode)
         (hClose)
         (\h -> Postmaster.Base.bracketOnError ctxCreate ctxDestroy $ \ctx -> do
            hSetBuffering h NoBuffering
            when (ctx == nullPtr) (fail "can't initialize SHA1 digest context")
            md <- toMDEngine SHA1
            when (md == nullPtr) (fail "can't initialize SHA1 digest engine")
            rc <- digestInit ctx md
            when (rc == 0) (fail "can't initialize SHA1 digest")
            return (S (Just path) (Just h) (DST ctx)))
     say 3 5 4 "terminate data with <CRLF>.<CRLF>"
  `fallback`
     say 4 5 1 "requested action aborted: error in processing"

handlePayload spool _ Deliver =
  do st <- getState
     sha1 <- liftIO $
       modifyMVar st $ \(S (Just p) (Just h) ctx@(DST c)) ->
         assert (c /= nullPtr) $ do
           hClose h
           sha1 <- fmap (>>= toHex) (evalStateT final ctx)
           let fname = spool ++ "/" ++ sha1
           renameFile p fname
           return (S Nothing Nothing ctx, sha1)
     say 2 5 0 (sha1 ++ " message accepted for delivery")
  `fallback`
     say 4 5 1 "requested action aborted: error in processing"

handlePayload _ f ResetState = clearState >> f ResetState
handlePayload _ f Shutdown   = clearState >> f Shutdown
handlePayload _ f e = f e

feeder :: DataHandler
feeder buf@(Buf _ _ 0) = return (Nothing, buf)
feeder buf@(Buf _ ptr n) = do
  xs <- liftIO (peekArray (fromIntegral n) ptr)
  let theEnd   = map (toEnum . fromEnum) "\r\n.\r\n"
      (eod, i) = case strstr theEnd xs of
                   Nothing -> (False, max 0 (n - 4))
                   Just j -> (True, fromIntegral (j-3))
      i'       = fromIntegral i
  st <- getState
  buf' <- liftIO . withMVar st $ \(S _ (Just h) ctx@(DST c)) ->
    assert (c /= nullPtr) $ do
      hPutBuf h ptr i'
      execStateT (update' (ptr, i')) ctx
      flush i buf
  if not eod then return (Nothing, buf') else do
    r <- trigger Deliver
    trigger ResetState          -- TODO: this doesn't really
    setSessionState HaveHelo    --       belong here
    return (Just r, buf')

clearState :: Smtpd ()
clearState = do
  st <- getState
  liftIO . modifyMVar_ st $ \(S path h (DST ctx)) -> do
    let clean Nothing  _ = return ()
        clean (Just x) f = try (f x) >> return ()
    clean h hClose
    clean path removeFile
    when (ctx /= nullPtr) (ctxDestroy ctx)
    return (S Nothing Nothing (DST nullPtr))
