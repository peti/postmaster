{-# OPTIONS -fglasgow-exts #-}
{- |
   Module      :  Postmaster.IO
   Copyright   :  (c) 2005-02-05 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.IO where

import Prelude hiding ( catch )
import Data.Maybe
import Data.Typeable
import Control.Exception
-- import Control.Concurrent.MVar
import Control.Monad.RWS hiding ( local )
import System.IO
-- import System.IO.Error hiding ( catch )
-- import System.Posix.Signals
-- import Network ( listenOn, PortID(..) )
-- import Network.Socket hiding ( shutdown )
-- import Network.DNS
-- import Foreign
import Postmaster.Base
-- import Postmaster.Event
import Rfc2821
-- import Syslog
import BlockIO
import Child
import MonadEnv



-- handleSmtpd :: WriteHandle -> Config -> SmtpdHandler
--             -> StreamHandler SmtpdState
-- handleSmtpd hOut cfg f xs st = do
--   let xs'  = map (toEnum . fromEnum) xs
--       ls'  = splitList "\r\n" xs'
--       ls   = reverse . tail $ reverse ls'
--       rest = head $ reverse ls'
--       i    = fromIntegral $ length xs - length rest
--   (rs', st', w) <- runRWST (mapM f ls) cfg st
--   let rs = catMaybes rs'
--   mapM_ syslogger w
--   writeReplies (writeTimeout st') hOut rs
--   let term (Reply (Code Success Connection 1) _)          = True
--       term (Reply (Code TransientFailure Connection 1) _) = True
--       term _                                              = False
--   when (any term rs) (fail "shutdown")
--   return (i, st')
--
-- runSmtpd :: Capacity -> ReadHandle -> WriteHandle -> Config -> IO ()
-- runSmtpd bufSize hIn hOut cfg = do
--   (r, st, w) <- runRWST (trigger eventHandler Greeting) cfg initSmtpd
--   mapM_ syslogger w
--   let to = writeTimeout st
--   writeReplies to hOut [r]
--   let Reply (Code rc _ _) _ = r
--   let logM = yellIO cfg st
--   when (rc == Success) $ do
--     let main = (handleStream . handleSmtpd hOut cfg) smtpdHandler
--     catch
--       (runLoopNB readTimeout hIn bufSize main st >> return ())
--       (\e -> case e of
--          IOException ie -> logM (CaughtIOError ie)
--          _              -> logM (CaughtException e))
--
--
-- -- |Run 'handleData' if the session is in 'HaveData' state,
-- -- run 'handleDialog' otherwise. Recurse until the entire
-- -- buffer has been processed. The replies returned by the
-- -- handers are written to the given 'Handle'. The stream is
-- -- flushed when the recursion ends, so for optimal
-- -- performance the 'Handle' should be in 'BlockBuffering'
-- -- mode. If the 'eventHandler' (or the 'dataHandler')
-- -- returns 221 or 421, drop the connection after writing the
-- -- reply.
--
-- smtpdHandler :: SmtpdHandler
-- smtpdHandler line = do
--   sst <- getSessionState
--   if sst == HaveData
--      then handleData (fixCRLF line)
--      else fmap Just (handleDialog (fixCRLF line))
--
-- -- -- |Use 'dataHandler' callback to process the input. If the
-- -- -- buffer contains @\<CRLF\>.\<CRLF\>@, trigger a 'Deliver'
-- -- -- event, then a 'ResetState' event, then return the result
-- -- -- of the delivery.
-- --
-- handleData :: SmtpdHandler
-- handleData line = undefined
-- --   buf <- liftIO (peekArray n ptr)
-- --   let eod = map (toEnum . fromEnum) "\r\n.\r\n"
-- --   case strstr eod buf of
-- --     Nothing -> do let n' = max 0 (n - 4)
-- --                   when (n' > 0) (trigger dataHandler (ptr, n'))
-- --                   gap <- gets ioBufferGap
-- --                   modify (\st -> st { ioBufferGap = gap + n' })
-- --                   return Nothing
-- --     Just i  -> do trigger dataHandler (ptr, (i-3))
-- --                   r <- trigger eventHandler Deliver
-- --                   trigger eventHandler ResetState
-- --                   setSessionState HaveHelo
-- --                   return (Just (r, i))
-- --
-- -- -- |If there is one, consume the first line from the buffer
-- -- -- and run it through 'smtpdFSM'. Then trigger the 'Event'
-- -- -- the machine returns and make the 'SessionState'
-- -- -- transition accordingly if the event handler returns
-- -- -- \"success\" (meaning: 1xx, 2xx, 3xx).
-- --
-- handleDialog :: String -> Smtpd SmtpReply
-- handleDialog line = undefined
-- --   buf <- liftIO (peekArray n ptr)
-- --   let crlf = map (toEnum . fromEnum) "\r\n"
-- --   case strstr crlf buf of
-- --     Nothing -> return Nothing
-- --     Just i  -> do
-- --       sst <- getSessionState
-- --       let line     = map (toEnum . fromEnum) (take i buf)
-- --           (e,sst') = runState (smtpdFSM line) sst
-- --       yell (Input line)
-- --       r <- trigger eventHandler e
-- --       case r of
-- --         Reply (Code Unused0 _ _) _          -> fail "Unused?"
-- --         Reply (Code TransientFailure _ _) _ -> return ()
-- --         Reply (Code PermanentFailure _ _) _ -> return ()
-- --         _                                   -> setSessionState sst'
-- --       return (Just (r, i))
--


-- smtpdMain :: (Config -> Config) -> PortID -> IO ()
-- smtpdMain f port = do
--   installHandler sigPIPE Ignore Nothing
-- --   withSocketsDo $
-- --     withSyslog "postmaster" [PID, PERROR] MAIL $ do
-- --       mkConfig $ \cfg ->
-- --         bracket (listenOn port) (sClose) $ \s -> do
-- --           setSocketOption s ReuseAddr 1
-- --           loop (f cfg) s
-- --   where
-- --   loop cfg ls = do
-- --     (s,sa) <- accept ls
-- --     spawn (main cfg (s,sa) `finally` sClose s)
-- --     loop cfg ls
-- --   main cfg (s,sa) = do
-- --     setSocketOption s KeepAlive 1
-- --     h <- socketToHandle s ReadWriteMode
-- --     let size = Just (fromIntegral ioBufferSize)
-- --     hSetBuffering h (BlockBuffering size)
-- --     let cfg' = cfg { peerAddr  = Just sa }
-- --     bracket_
-- --       (yellIO cfg' initSmtpd (AcceptConn sa))
-- --       (yellIO cfg' initSmtpd (DropConn sa) >> hClose h)
-- --       (runSmtpd ioBufferSize h h cfg')


-- * Non-blocking I\/O

-- |The exception we throw when writes time out.

data WriteTimeout = WriteTimeout Timeout
                  deriving (Typeable, Show)

setReadTimeout :: Timeout -> Smtpd ()
setReadTimeout = local . setval "ReadTimeout"

getReadTimeout :: Smtpd Timeout
getReadTimeout = local $ getDefault "ReadTimeout" (90 * 1000000)

setWriteTimeout :: Timeout -> Smtpd ()
setWriteTimeout = local . setval "WriteTimeout"

getWriteTimeout :: Smtpd Timeout
getWriteTimeout = local $ getDefault "WriteTimeout" (90 * 1000000)

safeWrite :: IO a -> Smtpd a
safeWrite f = do
  to <- getWriteTimeout
  liftIO $ timeout to f >>= maybe (throwDyn (WriteTimeout to)) return

safeReply :: WriteHandle -> SmtpReply -> Smtpd ()
safeReply hOut r = safeWrite (hPutStr hOut (show r))

safeFlush :: WriteHandle -> Smtpd ()
safeFlush hOut = safeWrite (hFlush hOut)


-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-ghci-program-args: ( "-ladns" "-lcrypto" ) ***
-- End: ***
