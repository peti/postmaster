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

data EsmtpdEnv m = EsmtpdEnv { _esmtpLogger :: LogAction m LogMsg, _esmtpPeer :: NetworkPeer m }

makeClassy ''EsmtpdEnv

instance HasLogAction (EsmtpdEnv m) m LogMsg where logAction = esmtpLogger
instance HasNetworkPeer (EsmtpdEnv m) m where networkPeer = esmtpPeer

data EsmtpdState = EsmtpdState { _myName :: HostName, _peerName :: HostName }

makeClassy ''EsmtpdState

type MonadEsmtp st m = (MonadState st m, HasEsmtpdState st)

newtype Esmtpd a = Esmtpd { runEsmtpd :: StateT EsmtpdState (ReaderT (EsmtpdEnv Esmtpd) IO) a }
  deriving ( Applicative, Functor, Monad, MonadIO
           , MonadReader (EsmtpdEnv Esmtpd)
           , MonadState EsmtpdState
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
    listener (Just "0.0.0.0", "2525") (acceptor esmtpdAcceptor)

esmtpdAcceptor :: MonadIO m => SocketHandler m
esmtpdAcceptor (sock,peerAddr) = do
  localAddr <- liftIO (getSocketName sock)
  (hn, _) <- liftIO (getNameInfo [] True False localAddr)
  (pn, _) <- liftIO (getNameInfo [] True False peerAddr)
  let myname = fromMaybe ("[" <> show localAddr <> "]") hn
      peername = fromMaybe ("[" <> show peerAddr <> "]") pn
  let env = EsmtpdEnv (logToHandle stderr) (socketIO sock)
      st = EsmtpdState myname peername
      ioLoop = logWithPrefix (display peerAddr <> ": ") (esmtpdIOLoop mempty)
      greeting = send (packBS8 (show (reply 2 2 0 [myname <> " Postmaster ESMTP Server"])))
  liftIO $ runReaderT (evalStateT (runEsmtpd (greeting >> ioLoop)) st) env

esmtpdIOLoop :: (MonadEsmtp st m, MonadPeer env m, MonadLog env m) => ByteString -> m ()
esmtpdIOLoop buf = do
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
                                                        esmtpdIOLoop line
                                   | otherwise    -> do logDebug $ "read line: " <> display line
                                                        resp <- esmtpdFSM (parseEsmtpCmd (line <> "\r\n") )
                                                        send (packBS8 (show resp))
                                                        unless (isShutdown resp) $
                                                          esmtpdIOLoop (BS.drop 2 rest)

parseEsmtpCmd :: ByteString -> EsmtpCmd
parseEsmtpCmd line = fromRight (SyntaxError (unpackBS8 line)) (parse (esmtpCmd <* eof) "" line)

esmtpdFSM :: MonadEsmtp st m => EsmtpCmd -> m EsmtpReply

esmtpdFSM Quit = do hn <- use myName
                    respond 2 2 1 [hn <> " Take it easy."]

esmtpdFSM (Helo _) = do hn <- use myName
                        pn <- use peerName
                        respond 2 5 0 [hn <> " Hello, " <> pn <> "."]

esmtpdFSM (Ehlo _) = do hn <- use myName
                        pn <- use peerName
                        respond 2 5 0 [hn <> " Hello, " <> pn <> ".", "PIPELINING", "STARTTLS"]

esmtpdFSM (SyntaxError _) = respond 5 0 0 ["syntax error: command not recognized"]
esmtpdFSM (WrongArg cmd) = respond 5 0 1 ["syntax error in argument of " <> cmd <> " command"]

esmtpdFSM cmd = respond 5 0 2 ["command " <> show cmd <> " not implemented"]

----- Helper functions

respond :: Monad m => Int -> Int -> Int -> [String] -> m EsmtpReply
respond x y z = pure . reply x y z

splitLine :: ByteString -> Maybe (ByteString, ByteString)
splitLine buf = case BS.breakSubstring "\r\n" buf of
                  (line,rest) | BS.null rest -> Nothing
                              | otherwise    -> Just (BS.splitAt (BS.length line + 2) buf)
