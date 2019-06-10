{- |
   Module:      Main
   Copyright:   (C) 2004-2019 Peter Simons
   License:     GNU AFFERO GPL v3 or later

   Maintainer:  simons@cryp.to
   Stability:   experimental
   Portability: non-portable
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import Postmaster

import qualified Data.ByteString as BS
import Network.Socket
import System.Exit
import System.Posix.Syslog as Syslog ( withSyslog, Facility(Mail) )
import Text.Parsec
import Text.Parsec.Rfc2821 hiding ( postmaster, send )

data EsmtpEnv m = EsmtpEnv { _esmtpLogger :: LogAction m LogMsg, _esmtpPeer :: NetworkPeer m }

makeClassy ''EsmtpEnv

instance HasLogAction (EsmtpEnv m) m LogMsg where logAction = esmtpLogger
instance HasNetworkPeer (EsmtpEnv m) m where networkPeer = esmtpPeer

data EsmtpState = EsmtpState

makeClassy ''EsmtpState

type MonadEsmtp st m = (MonadState st m, HasEsmtpState st)

newtype Esmtpd a = Esmtpd { runEsmtpd :: StateT EsmtpState (ReaderT (EsmtpEnv Esmtpd) IO) a }
  deriving ( Applicative, Functor, Monad, MonadIO
           , MonadReader (EsmtpEnv Esmtpd)
           , MonadState EsmtpState
           )


newtype Postmaster a = Postmaster { runPostmaster :: ReaderT (LogAction Postmaster LogMsg) IO a }
  deriving ( Applicative, Functor, Monad, MonadIO, MonadUnliftIO
           , MonadReader (LogAction Postmaster LogMsg)
           )

main :: IO ()
main =
  withSocketsDo $
    withSyslog "postmaster" [] Mail $
      runReaderT (runPostmaster postmaster) (logToHandle stderr)

postmaster :: (MonadUnliftIO m, MonadLog env m) => m ()
postmaster =
  handle (\e -> logError ("fatal error: " <> display (e::SomeException)) >> liftIO exitFailure) $ do
    logDebug "postmaster starting up ..."
    listener (Just "0.0.0.0", "2525") (acceptor esmtpd)

esmtpd :: MonadIO m => SocketHandler m
esmtpd (sock,addr) = do
  let env = EsmtpEnv (logToHandle stderr) (socketIO sock)
      st  = EsmtpState
  liftIO $ runReaderT (evalStateT (runEsmtpd (logWithPrefix (display addr <> ": ") (lineReader mempty))) st) env

lineReader :: (MonadEsmtp st m, MonadPeer env m, MonadLog env m) => ByteString -> m ()
lineReader buf = do
  let maxLineLength = 4096              -- TODO: magic constant
      maxReadSize = maxLineLength - BS.length buf
  if maxReadSize <= 0
     then logWarning $ "client exceeded maximum line length (" <> display maxLineLength <> " characters)"
     else do new <- recv (fromIntegral maxReadSize)
             logDebug $ "recv: " <> display new
             if BS.null new
                then logDebug $ "reached end of input (left-over in buffer: " <> display buf <> ")"
                else case BS.breakSubstring "\r\n" (buf <> new) of
                       (line,rest) | BS.null rest -> do logDebug $ "no complete line yet (buffer: " <> display line <> ")"
                                                        lineReader line
                                   | otherwise    -> do logDebug $ "read line: " <> display line
                                                        resp <- case parse smtpCmd "" buf of
                                                                  Left _ -> return (reply 5 5 0 ["command not recognized"])
                                                                  Right cmd -> esmtpdFSM cmd
                                                        send (packBS8 (show resp))
                                                        lineReader (BS.drop 2 rest)

esmtpdFSM :: Monad m => EsmtpCmd -> m EsmtpReply
esmtpdFSM _ = return $ reply 2 2 0 ["everything is OK"]
