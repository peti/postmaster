{- |
   Module      :  Postmaster.FSM.Spooler
   Copyright   :  (c) 2005-02-10 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.FSM.Spooler where

import Foreign
import Control.Exception
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
import Digest

sha1Engine :: SmtpdVariable
sha1Engine = defineLocal "sha1engine"

spoolName :: SmtpdVariable
spoolName = defineLocal "spoolname"

spoolHandle :: SmtpdVariable
spoolHandle = defineLocal "spoolhandle"


-- |The Standard Bad-Ass Payload Handler

handlePayload :: FilePath -> EventT

handlePayload spool _ StartData =
  do mid <- getMailID
     let path = spool ++ "/temp." ++ show mid
     (h, sha1) <- liftIO $ bracketOnError
       (openBinaryFile path WriteMode)
       (hClose)
       (\h -> bracketOnError ctxCreate ctxDestroy $ \ctx -> do
          when (ctx == nullPtr) (fail "can't initialize SHA1 digest context")
          md <- toMDEngine SHA1
          when (md == nullPtr) (fail "can't initialize SHA1 digest engine")
          rc <- digestInit ctx md
          when (rc == 0) (fail "can't initialize SHA1 digest")
          return (h, DST ctx))
     spoolHandle (`setVar` h)
     spoolName (`setVar` path)
     sha1Engine (`setVar` sha1)
     setDataHandler (feeder h)
     say 3 5 4 "terminate data with <CRLF>.<CRLF>"
  `fallback`
     say 4 5 1 "requested action aborted: error in processing"

handlePayload spool _ Deliver =
  do spoolHandle getVar_ >>= liftIO . hClose >> spoolHandle unsetVar
     ctx <- sha1Engine getVar_
     sha1 <- fmap (>>= toHex) (liftIO (evalStateT final ctx))
     fname <- spoolName getVar_
     let fname' = spool ++ "/" ++ sha1
     liftIO (renameFile fname fname')
     say 2 5 0 (sha1 ++ " message accepted for delivery")
  `fallback`
     say 4 5 1 "requested action aborted: error in processing"

handlePayload _ f ResetState = cleanupSpool >> f ResetState
handlePayload _ f Shutdown   = cleanupSpool >> f Shutdown
handlePayload _ f e = f e

feeder :: WriteHandle -> DataHandler
feeder _ buf@(Buf _ _ 0) = return (Nothing, buf)
feeder hOut buf@(Buf _ ptr n) = do
  xs <- liftIO (peekArray (fromIntegral n) ptr)
  let theEnd   = map (toEnum . fromEnum) "\r\n.\r\n"
      (eod, i) = case strstr theEnd xs of
                   Nothing -> (False, max 0 (n - 4))
                   Just j -> (True, fromIntegral (j-3))
      i'       = fromIntegral i
  liftIO (hPutBuf hOut ptr i')
  sha1Engine getVar_
    >>= liftIO . execStateT (update' (ptr, i'))
    >>= sha1Engine . flip setVar
  buf' <- liftIO (flush i buf)
  if not eod then return (Nothing, buf') else do
    r <- trigger Deliver
    trigger ResetState
    setSessionState HaveHelo
    return (Just r, buf')

cleanupSpool :: Smtpd ()
cleanupSpool = do
  sha1  <- sha1Engine getVar
  h     <- spoolHandle getVar
  fname <- spoolName getVar
  mapM ($ unsetVar) [ sha1Engine, spoolHandle, spoolName, dataHandler ]
  liftIO $ do
    let clean Nothing  _ = return ()
        clean (Just x) f = try (f x) >> return ()
    clean sha1 (\(DST ctx) -> ctxDestroy ctx)
    clean h hClose
    clean fname removeFile
