{-# LANGUAGE DeriveDataTypeable #-}
{- |
   Module      :  Postmaster.FSM.Spooler
   Copyright   :  (c) 2004-2008 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.Spooler where

import Foreign
import Control.Exception
import Control.Concurrent.MVar
import Control.Monad.State
import System.Directory
import System.IO
import Postmaster.Base
import Postmaster.IO
import Postmaster.FSM.EventHandler
import Postmaster.FSM.SessionState
import Postmaster.FSM.DataHandler
import Postmaster.FSM.MailID
import Text.Parsec.Rfc2821 hiding ( path )
import OpenSSL.EVP.Digest
import Data.Typeable

data Spooler = S (Maybe FilePath) (Maybe WriteHandle) (Ptr OpaqueDigestContext)
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
  liftIO (newMVar (S Nothing Nothing nullPtr)) >>= setState
  setDataHandler feeder
  f Greeting

handlePayload spool _ StartData =
  do st <- getState
     mid <- getMailID
     let path = spool ++ "/temp." ++ show mid
     liftIO . modifyMVar_ st $ \(S p' h' c') ->
       assert (p' == Nothing) $
       assert (h' == Nothing) $
       assert (c' == nullPtr) $
       bracketOnError
         (openBinaryFile path WriteMode)
         (hClose)
         (\h -> bracketOnError _createContext _destroyContext $ \ctx -> do
            hSetBuffering h NoBuffering
            when (ctx == nullPtr) (fail "can't initialize SHA1 digest context")
            let DigestDescription md = digestByName "SHA1"
            rc <- _initDigest ctx md nullPtr
            when (rc == 0) (fail "can't initialize SHA1 digest")
            return (S (Just path) (Just h) ctx))
     say 3 5 4 "terminate data with <CRLF>.<CRLF>"
  `fallback`
     say 4 5 1 "requested action aborted: error in processing"

handlePayload spool _ Deliver =
  do st <- getState
     sha1 <- liftIO $
       modifyMVar st $ \(S (Just p) (Just h) ctx) ->
         assert (ctx /= nullPtr) $ do
           hClose h
           sha1 <- alloca $ \sizePtr ->
             allocaArray maxDigestSize $ \mdPtr -> do
               _finalizeDigest ctx mdPtr sizePtr
               len <- peek sizePtr
               md <- peekArray (fromIntegral len) mdPtr
               return (md >>= toHex)
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
  buf' <- liftIO . withMVar st $ \(S _ (Just h) ctx) ->
    assert (ctx /= nullPtr) $ do
      hPutBuf h ptr i'
      _ <- _updateDigest ctx (castPtr ptr) (fromIntegral i')
      flush i buf
  if not eod then return (Nothing, buf') else do
    r <- trigger Deliver
    _ <- trigger ResetState          -- TODO: this doesn't really
    setSessionState HaveHelo         --       belong here
    return (Just r, buf')

clearState :: Smtpd ()
clearState = do
  st <- getState
  liftIO . modifyMVar_ st $ \(S path h ctx) -> do
    let clean Nothing  _ = return ()
        clean (Just x) f = (try (f x) :: IO (Either SomeException ())) >> return ()
    clean h hClose
    clean path removeFile
    when (ctx /= nullPtr) (_destroyContext ctx)
    return (S Nothing Nothing nullPtr)
