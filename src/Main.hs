{- |
   Module:      Main
   Copyright:   (C) 2004-2019 Peter Simons
   License:     GNU AFFERO GPL v3 or later

   Maintainer:  simons@cryp.to
   Stability:   experimental
   Portability: non-portable
 -}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Paths_postmaster ( version )
import Postmaster
import Postmaster.Rfc2821 hiding ( postmaster, send, help )  -- TOOD: add to prelude

import Control.Exception ( AssertionFailed(..) )
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List as List
import Data.Version
import Network.Socket
import Network.TLS as TLS hiding ( HostName )
import Network.TLS.Extra.Cipher as TLS
import Network.TLS.SessionManager as TLS
import Options.Applicative
import System.Exit
import System.Posix.Syslog as Syslog ( withSyslog, Facility(Mail) )
import System.X509
import Text.Parsec ( parse, eof )

data TlsState = NotSupported | Unused | Connected

data EsmtpdEnv m = EsmtpdEnv { _esmtpdLogger :: LogAction m LogMsg
                             , _esmtpdPeer :: NetworkPeer m
                             , _esmtpdTlsState :: TlsState
                             }

makeClassy ''EsmtpdEnv

instance HasLogAction (EsmtpdEnv m) m LogMsg where logAction = esmtpdLogger
instance HasNetworkPeer (EsmtpdEnv m) m where networkPeer = esmtpdPeer

data SessionState = Initial
                  | HaveEhlo { _ehloName :: String }
                  | HaveMailFrom { _ehloName :: String, _envelope :: Mailbox }
                  | HaveRcptTo { _ehloName :: String, _envelope :: Mailbox, _rcptTo :: [Mailbox] }
  deriving (Show)

makeClassy ''SessionState
makePrisms ''SessionState

data ProtocolState = CommandPhase | DataPhase
  deriving (Show)

makeClassy ''ProtocolState
makePrisms ''ProtocolState

data EsmtpdState = EsmtpdState { _myName :: HostName, _peerName :: HostName
                               , _ioState :: ProtocolState
                               , _session :: SessionState
                               }
  deriving (Show)

makeClassy ''EsmtpdState

instance HasSessionState EsmtpdState where
  sessionState = session

type MonadEsmtpd env st m = ( MonadReader env m, HasEsmtpdEnv env m
                            , MonadState st m, HasEsmtpdState st, HasSessionState st
                            , MonadResource m
                            )

newtype Esmtpd a = Esmtpd { runEsmtpd :: StateT EsmtpdState (ReaderT (EsmtpdEnv Esmtpd) (ResourceT IO)) a }
  deriving ( Applicative, Functor, Monad, MonadIO, MonadResource
           , MonadReader (EsmtpdEnv Esmtpd)
           , MonadState EsmtpdState
           )

newtype Postmaster a = Postmaster { runPostmaster :: ReaderT (LogAction Postmaster LogMsg) IO a }
  deriving ( Applicative, Functor, Monad, MonadIO, MonadUnliftIO
           , MonadReader (LogAction Postmaster LogMsg)
           )

data CliOptions = CliOptions
  { listenAddressStrings :: [String]
  , tlsCertFile :: Maybe FilePath
  , tlsKeyFile :: Maybe FilePath
  }
  deriving (Show)

data Configuration = Configuration
  { listenAddresses :: [(Maybe HostName, ServiceName)]
  , tlsServerParams :: Maybe ServerParams
  }
  deriving (Show)

cliOptions :: Parser CliOptions
cliOptions = do
  listenAddressStrings <- many (strOption $ long "listen" <> metavar "ADDR-SPEC" <> help "Accept incoming connections on this address. Can be specified multiple times.")
  tlsCertFile <- optional (strOption $ long "tls-cert" <> metavar "PATH" <> help "The server's TLS certificate.")
  tlsKeyFile <- optional (strOption $ long "tls-key" <> metavar "PATH" <> help "The server's TLS private key.")
  pure CliOptions {..}

cli :: ParserInfo CliOptions
cli = info
      (   helper
      <*> infoOption ("postmaster version " ++ showVersion version) (long "version" <> help "Show version number")
      <*> cliOptions
      )
      (briefDesc <> header ("Postmaster ESMTP Server version " ++ showVersion version))

main :: IO ()
main =
  withSocketsDo $
    withSyslog "postmaster" [] Mail $ do
      cfg <- execParser cli >>= makeConfiguration
      runReaderT (runPostmaster (postmaster cfg)) (logToHandle stderr)

postmaster :: (MonadUnliftIO m, MonadLog env m) => Configuration -> m ()
postmaster cfg =
  handle (\e -> logError ("fatal error: " <> display (e::SomeException)) >> liftIO exitFailure) $ do
    logDebug "postmaster starting up ..."
    logInfo $ display cfg
    mapConcurrently_ (`listener` acceptor (esmtpdAcceptor (tlsServerParams cfg))) (listenAddresses cfg)

esmtpdAcceptor :: MonadUnliftIO m => Maybe ServerParams -> SocketHandler m
esmtpdAcceptor tlsParams (sock,peerAddr) = do
  localAddr <- liftIO (getSocketName sock)
  (hn, _) <- liftIO (getNameInfo [] True False localAddr)
  (pn, _) <- liftIO (getNameInfo [] True False peerAddr)
  let myname = fromMaybe ("[" <> show localAddr <> "]") hn
      peername = fromMaybe ("[" <> show peerAddr <> "]") pn
  let env = EsmtpdEnv (logToHandle stderr) (socketIO sock) (if isJust tlsParams then Unused else NotSupported)
      st = EsmtpdState myname peername CommandPhase Initial
      ioLoop = logWithPrefix (display peerAddr <> ": ") (esmtpdIOLoop mempty)
      greeting = send (packBS8 (show (reply 2 2 0 [myname <> " Postmaster ESMTP Server"])))
  ioact <- liftIO $ runResourceT $ runReaderT (evalStateT (runEsmtpd (greeting >> ioLoop)) st) env
  case ioact of
    StartTls -> case tlsParams of
                  Nothing -> throwIO (AssertionFailed "STARTTLS triggered even though TLS is unsupported")
                  Just tlsp -> esmtpdAcceptorTls tlsp env st (sock,peerAddr)
    Shutdown -> return ()

esmtpdAcceptorTls :: MonadUnliftIO m => ServerParams -> EsmtpdEnv Esmtpd -> EsmtpdState -> SocketHandler m
esmtpdAcceptorTls tlsParams env st (sock,peerAddr) =
  bracket (contextNew sock tlsParams) bye $ \ctx -> do
    -- contextHookSetLogging ctx tlsLogging
    handshake ctx
    let ioLoop = logWithPrefix (display peerAddr <> ": TLS: ") (esmtpdIOLoop mempty)
        env' = env & esmtpdPeer .~ tlsIO ctx
                   & esmtpdTlsState .~ Connected
    ioact <- liftIO $ runResourceT $ runReaderT (evalStateT (runEsmtpd ioLoop) st) env'
    case ioact of
      StartTls -> throwIO (AssertionFailed "STARTTLS triggered twice")
      Shutdown -> return ()

esmtpdIOLoop :: (MonadResource m, MonadEsmtpd env st m, MonadPeer env m, MonadLog env m) => ByteString -> m EsmtpdIOAction
esmtpdIOLoop buf = do
  let (line,rest) = BS.breakSubstring "\r\n" buf
  if BS.null rest
     then do logDebug $ "no complete line yet (buffer: " <> display line <> ")"
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
                           else esmtpdIOLoop (buf <> new)
     else do logDebug $ "read line: " <> display line
             r <- use ioState >>= \case
                CommandPhase -> Just <$> esmtpdFSM (parseEsmtpCmd (line <> "\r\n"))
                DataPhase    -> esmtpdDataReader (line <> "\r\n")
             ioact <- case r of Just (ioact,resp) -> send (packBS8 (show resp)) >> return ioact
                                Nothing           -> return Nothing
             case ioact of
               Nothing  -> esmtpdIOLoop (BS.drop 2 rest)
               Just act -> return act

data EsmtpdIOAction = StartTls | Shutdown
  deriving (Show)

{-# ANN esmtpdFSM ("HLint: ignore Reduce duplication" :: String) #-}

esmtpdFSM :: MonadEsmtpd env st m => EsmtpCmd -> m (Maybe EsmtpdIOAction,  EsmtpReply)

esmtpdFSM Quit = do hn <- use myName
                    respond' Shutdown 2 2 1 [hn <> " Take it easy."]

esmtpdFSM StartTLS = ifM supportStartTls (respond' StartTls 2 2 0 ["starting TLS"])
                                         (respond 5 5 4 ["cannot start TLS"])

esmtpdFSM (Helo name) = do hn <- use myName
                           pn <- use peerName
                           session .= HaveEhlo name
                           respond 2 5 0 [hn <> " Hello, " <> pn <> "."]

esmtpdFSM (Ehlo name) = do hn <- use myName
                           pn <- use peerName
                           session .= HaveEhlo name
                           withTls <- supportStartTls
                           respond 2 5 0 $ [hn <> " Hello, " <> pn <> ".", "PIPELINING"] ++ ["STARTTLS" | withTls]

esmtpdFSM Noop = respond 2 5 0 ["OK"]

esmtpdFSM Rset = do name <- use ehloName
                    unless (null name)  $
                      session .= HaveEhlo name
                    respond 2 5 0 ["OK"]

esmtpdFSM (MailFrom addr) = use session >>= \case
  Initial           -> respond 5 0 3 ["please send HELO or EHLO first"]
  HaveEhlo {..}     -> session .= (HaveMailFrom {_envelope = addr, ..}) >> respond 2 5 0 ["OK"]
  HaveMailFrom {..} -> respond 5 0 3 ["nested MAIL command"]
  HaveRcptTo {..}   -> respond 5 0 3 ["nested MAIL command"]

esmtpdFSM (RcptTo addr) = use session >>= \case
  Initial           -> respond 5 0 3 ["please send HELO or EHLO first"]
  HaveEhlo {..}     -> respond 5 0 3 ["please start MAIL session first"]
  HaveMailFrom {..} -> session .= (HaveRcptTo {_rcptTo = [addr], ..}) >> respond 2 5 0 ["OK"]
  HaveRcptTo {..}   -> rcptTo %= (:) addr >> respond 2 5 0 ["OK"]

esmtpdFSM Data = use session >>= \case
  Initial           -> respond 5 0 3 ["please send HELO or EHLO first"]
  HaveEhlo {..}     -> respond 5 0 3 ["please start MAIL session first"]
  HaveMailFrom {..} -> respond 5 0 3 ["please add a RCPT address first"]
  HaveRcptTo {..}   -> ioState .= DataPhase >> respond 3 5 4 ["enter mail and end with \".\" on a line by itself"]

esmtpdFSM (SyntaxError _) = respond 5 0 0 ["syntax error: command not recognized"]
esmtpdFSM (WrongArg cmd) = respond 5 0 1 ["syntax error in argument of " <> cmd <> " command"]

esmtpdFSM cmd = respond 5 0 2 ["command " <> show cmd <> " not implemented"]


esmtpdDataReader :: MonadEsmtpd env st m => ByteString -> m (Maybe (Maybe EsmtpdIOAction,  EsmtpReply))
esmtpdDataReader ""       = throwIO (AssertionFailed "esmtpdDataReader is not supposed to get an empty line")
esmtpdDataReader ".\r\n"  = ioState .= CommandPhase >> Just <$> esmtpdFSM Rset
esmtpdDataReader line
  | BS8.head line == '.'  = return Nothing
  | otherwise             = return Nothing


----- Helper functions

supportStartTls :: MonadEsmtpd env st m => m Bool
supportStartTls = views esmtpdTlsState $ \case
                    NotSupported -> False
                    Unused       -> True
                    Connected    -> False

makeConfiguration :: CliOptions -> IO Configuration
makeConfiguration CliOptions {..} = do
  let listenAddresses
        | null listenAddressStrings = [(Nothing, "25")]
        | otherwise                 = map parseListenAddr listenAddressStrings
  tlsServerParams <- case (tlsCertFile, tlsKeyFile) of
    (Just cf, Just kf) -> credentialLoadX509 cf kf >>=
                            \case Left err   -> Postmaster.fail ("cannot load certificate: " ++ err)
                                  Right cred -> Just <$> makeTlsServerParams cred
    _                  -> return Nothing
  pure Configuration {..}

makeTlsServerParams :: Credential -> IO ServerParams
makeTlsServerParams cred = do
  store <- getSystemCertificateStore
  smgr <- newSessionManager defaultConfig
  return $ def
   { serverShared = def { sharedSessionManager  = smgr
                        , sharedCAStore         = store
                        , sharedCredentials     = Credentials [cred]
                        }
   , serverSupported = def { supportedCiphers = ciphersuite_default
                           , supportedHashSignatures = delete (HashIntrinsic, SignatureRSApssRSAeSHA512) (supportedHashSignatures def)
                           }
   }

parseEsmtpCmd :: ByteString -> EsmtpCmd
parseEsmtpCmd line = fromRight (SyntaxError (unpackBS8 line)) (parse (esmtpCmd <* eof) "" line)

respond :: Monad m => Int -> Int -> Int -> [String] -> m (Maybe EsmtpdIOAction, EsmtpReply)
respond x y z msg = return (Nothing, reply x y z msg)

respond' :: Monad m => EsmtpdIOAction -> Int -> Int -> Int -> [String] -> m (Maybe EsmtpdIOAction, EsmtpReply)
respond' ioact x y z msg = return (Just ioact, reply x y z msg)

splitLine :: ByteString -> Maybe (ByteString, ByteString)
splitLine buf = case BS.breakSubstring "\r\n" buf of
                  (line,rest) | BS.null rest -> Nothing
                              | otherwise    -> Just (BS.splitAt (BS.length line + 2) buf)


tlsIO :: MonadIO m => Context -> NetworkPeer m
tlsIO ctx = NetworkPeer (const (TLS.recvData ctx)) (TLS.sendData ctx . fromStrict)

-- | Parse a listen address specification into a (host name, service name)
-- tuple suitable for resolution with 'getAddrInfo'.
--
-- >>> parseListenAddr "0.0.0.0:25"
-- (Just "0.0.0.0","25")
--
-- >>> parseListenAddr ":25"
-- (Nothing,"25")
--
-- >>> parseListenAddr "localhost:smtp"
-- (Just "localhost","smtp")
--
-- >>> parseListenAddr ":::25"
-- (Just "::","25")
--
-- >>> parseListenAddr "25"
-- (Nothing,"25")

parseListenAddr :: String -> (Maybe HostName, ServiceName)
parseListenAddr buf = case break (==':') (reverse buf) of
  (sn,hn) | hn `elem` ["",":"] -> (Nothing, reverse sn)
          | otherwise          -> (Just (reverse (drop 1 hn)), reverse sn)
