{-# LANGUAGE LambdaCase #-}
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

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Default.Class
import Data.X509.CertificateStore
import Network.Socket
import Network.TLS as TLS hiding ( HostName )
import Network.TLS.Extra.Cipher as TLS
import Network.TLS.SessionManager as TLS
import System.Exit
import System.Posix.Syslog as Syslog ( withSyslog, Facility(Mail) )
import System.Timeout
import System.X509
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
    tlsParams <- liftIO makeTlsServerParams
    listener (Just "0.0.0.0", "2525") (acceptor (esmtpdAcceptor tlsParams))

esmtpdAcceptor :: MonadUnliftIO m => ServerParams -> SocketHandler m
esmtpdAcceptor tlsParams (sock,peerAddr) = do
  localAddr <- liftIO (getSocketName sock)
  (hn, _) <- liftIO (getNameInfo [] True False localAddr)
  (pn, _) <- liftIO (getNameInfo [] True False peerAddr)
  let myname = fromMaybe ("[" <> show localAddr <> "]") hn
      peername = fromMaybe ("[" <> show peerAddr <> "]") pn
  let env = EsmtpdEnv (logToHandle stderr) (socketIO sock)
      st = EsmtpdState myname peername
      ioLoop = logWithPrefix (display peerAddr <> ": ") (esmtpdIOLoop mempty)
      greeting = send (packBS8 (show (reply 2 2 0 [myname <> " Postmaster ESMTP Server"])))
  ioact <- liftIO $ runReaderT (evalStateT (runEsmtpd (greeting >> ioLoop)) st) env
  case ioact of
    StartTls -> esmtpdAcceptorTls tlsParams env st (sock,peerAddr)
    Shutdown -> return ()

esmtpdAcceptorTls :: MonadUnliftIO m => ServerParams -> EsmtpdEnv Esmtpd -> EsmtpdState -> SocketHandler m
esmtpdAcceptorTls tlsParams env st (sock,peerAddr) = do
  bracket (contextNew sock tlsParams) bye $ \ctx -> do
    -- contextHookSetLogging ctx tlsLogging
    handshake ctx
    let ioLoop = logWithPrefix (display peerAddr <> ": TLS: ") (esmtpdIOLoop mempty)
        env' = set esmtpPeer (tlsIO ctx) env
    ioact <- liftIO $ runReaderT (evalStateT (runEsmtpd ioLoop) st) env'
    case ioact of
      StartTls -> undefined -- TODO
      Shutdown -> return ()

esmtpdIOLoop :: (MonadIO m, MonadEsmtp st m, MonadPeer env m, MonadLog env m) => ByteString -> m EsmtpdIOAction
esmtpdIOLoop buf = do
  let maxLineLength = 4096              -- TODO: magic constant
      maxReadSize = maxLineLength - BS.length buf
  if maxReadSize <= 0
     then do logWarning $ "client exceeded maximum line length (" <> display maxLineLength <> " characters)"
             return Shutdown
     else do new <- recv (fromIntegral maxReadSize)
             logDebug $ "recv: " <> display new
             if BS.null new
                then do logDebug $ "reached end of input (left-over in buffer: " <> display buf <> ")"
                        return Shutdown
                else case BS.breakSubstring "\r\n" (buf <> new) of
                       (line,rest) | BS.null rest -> do logDebug $ "no complete line yet (buffer: " <> display line <> ")"
                                                        esmtpdIOLoop line
                                   | otherwise    -> do logDebug $ "read line: " <> display line
                                                        (ioact,resp) <- esmtpdFSM (parseEsmtpCmd (line <> "\r\n") )
                                                        send (packBS8 (show resp))
                                                        case ioact of
                                                          Nothing -> esmtpdIOLoop (BS.drop 2 rest)
                                                          Just act -> return act


data EsmtpdIOAction = StartTls | Shutdown
  deriving (Show)

parseEsmtpCmd :: ByteString -> EsmtpCmd
parseEsmtpCmd line = fromRight (SyntaxError (unpackBS8 line)) (parse (esmtpCmd <* eof) "" line)

esmtpdFSM :: MonadEsmtp st m => EsmtpCmd -> m (Maybe EsmtpdIOAction,  EsmtpReply)

esmtpdFSM Quit = do hn <- use myName
                    respond' Shutdown 2 2 1 [hn <> " Take it easy."]

esmtpdFSM StartTLS = respond' StartTls 2 2 0 ["starting TLS"]

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

makeTlsServerParams :: IO ServerParams
makeTlsServerParams = do
  cred <- loadCred
  store <- getSystemCertificateStore
  smgr <- newSessionManager defaultConfig
  return $ def
   { serverShared = def { sharedSessionManager  = smgr
                        , sharedCAStore         = store
                        , sharedCredentials     = Credentials [cred]
                        }
   , serverSupported = def { supportedCiphers = ciphersuite_default }
   }

respond :: Monad m => Int -> Int -> Int -> [String] -> m (Maybe EsmtpdIOAction, EsmtpReply)
respond x y z msg = return (Nothing, reply x y z msg)

respond' :: Monad m => EsmtpdIOAction -> Int -> Int -> Int -> [String] -> m (Maybe EsmtpdIOAction, EsmtpReply)
respond' ioact x y z msg = return (Just ioact, reply x y z msg)

splitLine :: ByteString -> Maybe (ByteString, ByteString)
splitLine buf = case BS.breakSubstring "\r\n" buf of
                  (line,rest) | BS.null rest -> Nothing
                              | otherwise    -> Just (BS.splitAt (BS.length line + 2) buf)

loadCred :: IO Credential
loadCred = do
  credentialLoadX509 "/home/simons/src/peti-ca/latitude-cert.pem"
                     "/home/simons/src/peti-ca/latitude-key.pem"
  >>= \case Left err -> Postmaster.fail ("cannot load certificate: " ++ err)
            Right v  -> return v


tlsIO :: MonadIO m => Context -> NetworkPeer m
tlsIO ctx = NetworkPeer (const (TLS.recvData ctx)) (TLS.sendData ctx . fromStrict)
