{- |
   Module:      Main
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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main ( main ) where

import Paths_postmaster ( version )
import Postmaster
import Postmaster.Esmtpd        -- TOOD: Add to prelude?

import Data.Version
import Data.List
import Network.Socket
import Network.TLS as TLS hiding ( HostName )
import Network.TLS.Extra.Cipher as TLS
import Network.TLS.SessionManager as TLS
import Options.Applicative
import System.Exit
import System.Posix.Syslog as Syslog ( withSyslog, Facility(Mail) )
import System.X509

newtype Postmaster a = Postmaster { runPostmaster :: ReaderT (LogAction Postmaster LogMsg) IO a }
  deriving ( Applicative, Functor, Monad, MonadIO, MonadUnliftIO
           , MonadReader (LogAction Postmaster LogMsg)
           )

data CliOptions = CliOptions
  { listenAddressStrings :: [String]
  , tlsCertFile :: Maybe FilePath
  , tlsKeyFile :: Maybe FilePath
  , spoolDir :: FilePath
  }
  deriving (Show)

data Configuration = Configuration
  { listenAddresses :: [(Maybe HostName, ServiceName)]
  , tlsServerParams :: Maybe ServerParams
  , spoolDir :: FilePath
  }
  deriving (Show)

cliOptions :: Parser CliOptions
cliOptions = do
  listenAddressStrings <- many (strOption $ long "listen" <> metavar "ADDR-SPEC" <> help "Accept incoming connections on this address. Can be specified multiple times.")
  tlsCertFile <- optional (strOption $ long "tls-cert" <> metavar "PATH" <> help "The server's TLS certificate.")
  tlsKeyFile <- optional (strOption $ long "tls-key" <> metavar "PATH" <> help "The server's TLS private key.")
  spoolDir <- strOption (long "spool-dir" <> metavar "PATH" <> help "Path to the mail spool directory." <> value "/var/spool/mqueue")
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
postmaster cfg@Configuration {..} =
  handle (\e -> logError ("fatal error: " <> display (e::SomeException)) >> liftIO exitFailure) $ do
    logDebug "postmaster starting up ..."
    logInfo $ display cfg
    mapConcurrently_ (`listener` acceptor (esmtpdAcceptor tlsServerParams spoolDir)) listenAddresses

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

----- Helper functions

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
