{- |
   Module:      Postmaster.Esmptd
   Copyright:   (C) 2004-2025 Peter Simons
   License:     GNU AFFERO GPL v3 or later

   Maintainer:  simons@cryp.to
   Stability:   experimental
   Portability: non-portable
 -}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Postmaster.Esmtpd ( esmtpdAcceptor ) where

import Postmaster.IO
import Postmaster.Log
import Postmaster.Prelude
import Postmaster.Rfc2821 hiding ( postmaster, send, help, path )  -- TOOD: add to prelude

import Control.Exception ( AssertionFailed(..) )
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Network.Socket
import Network.TLS as TLS hiding ( HostName )
import Text.Parsec ( parse, eof )

data TlsState = NotSupported | Unused | Connected

data EsmtpdEnv m = EsmtpdEnv { _esmtpdLogger :: LogAction m LogMsg
                             , _esmtpdPeer :: NetworkPeer m
                             , _esmtpdTlsState :: TlsState
                             , _esmtpdSpoolDir :: FilePath
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

data EsmtpdState = EsmtpdState { _myName :: HostName, _peerName :: HostName
                               , _ioState :: ProtocolState
                               , _session :: SessionState
                               , _spoolFile :: Maybe (ReleaseKey,(FilePath,Handle))
                               }

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

esmtpdAcceptor :: MonadUnliftIO m => Maybe ServerParams -> FilePath -> SocketHandler m
esmtpdAcceptor tlsParams spoolDir (sock,peerAddr) = do
  localAddr <- liftIO (getSocketName sock)
  myname <- resolvePtr localAddr >>= maybe (showAddressLiteral localAddr) return
  peername <- resolvePtr peerAddr >>= maybe (showAddressLiteral peerAddr) return
  let env = EsmtpdEnv (logToHandle stderr) (socketIO sock) (if isJust tlsParams then Unused else NotSupported) spoolDir
      st = EsmtpdState myname peername CommandPhase Initial Nothing
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
  HaveRcptTo {..}   -> do spoolDir <- view esmtpdSpoolDir
                          sf@(_,(_,fh)) <- allocate (openTempFile spoolDir "message.tmp") (\(path,fh) -> hClose fh >> whenM (doesFileExist path) (removeFile path))
                          liftIO (hPutStr fh (shows (_envelope,_rcptTo) "\r\n"))
                          spoolFile .= Just sf
                          ioState .= DataPhase
                          respond 3 5 4 ["enter mail and end with \".\" on a line by itself"]

esmtpdFSM (SyntaxError _) = respond 5 0 0 ["syntax error: command not recognized"]
esmtpdFSM (WrongArg cmd) = respond 5 0 1 ["syntax error in argument of " <> cmd <> " command"]

esmtpdFSM cmd = respond 5 0 2 ["command " <> show cmd <> " not implemented"]

esmtpdDataReader :: MonadEsmtpd env st m => ByteString -> m (Maybe (Maybe EsmtpdIOAction,  EsmtpReply))
esmtpdDataReader ""       = throwIO (AssertionFailed "esmtpdDataReader is not supposed to get an empty line")
esmtpdDataReader ".\r\n"  = do (relkey,(path,fh)) <- getSpoolFile
                               liftIO (hClose fh >> renameFile path (dropExtension path))
                               release relkey
                               spoolFile .= Nothing
                               ioState .= CommandPhase
                               Just <$> esmtpdFSM Rset
esmtpdDataReader line = do fh <- snd . snd <$> getSpoolFile
                           liftIO (BS.hPutStr fh (if BS8.head line == '.' then BS.drop 1 line else line))
                           return Nothing

getSpoolFile :: MonadEsmtpd env st m => m (ReleaseKey, (FilePath, Handle))
getSpoolFile = use spoolFile >>= maybe err return
  where err = throwIO (AssertionFailed "spoolFile in unset in the middle of a DATA transmission")

----- Helper functions

supportStartTls :: MonadEsmtpd env st m => m Bool
supportStartTls = views esmtpdTlsState $ \case
                    NotSupported -> False
                    Unused       -> True
                    Connected    -> False

parseEsmtpCmd :: ByteString -> EsmtpCmd
parseEsmtpCmd line = fromRight (SyntaxError (unpackBS8 line)) (parse (esmtpCmd <* eof) "" line)

respond :: Monad m => Int -> Int -> Int -> [String] -> m (Maybe EsmtpdIOAction, EsmtpReply)
respond x y z msg = return (Nothing, reply x y z msg)

respond' :: Monad m => EsmtpdIOAction -> Int -> Int -> Int -> [String] -> m (Maybe EsmtpdIOAction, EsmtpReply)
respond' ioact x y z msg = return (Just ioact, reply x y z msg)

tlsIO :: MonadIO m => Context -> NetworkPeer m
tlsIO ctx = NetworkPeer (const (TLS.recvData ctx)) (TLS.sendData ctx . fromStrict)
