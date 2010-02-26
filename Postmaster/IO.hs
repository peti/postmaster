{-# LANGUAGE DeriveDataTypeable #-}
{- |
   Module      :  Postmaster.IO
   Copyright   :  (c) 2004-2008 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.IO where

import Data.List
import Data.Dynamic             ( Typeable )
import Control.Concurrent       ( forkIO )
import Control.Exception
import Control.Monad.RWS hiding ( local )
import System.IO
import System.IO.Error
import System.Timeout
import Network                  ( listenOn, PortID(..) )
import Network.Socket
import Text.ParserCombinators.Parsec.Rfc2821
import Foreign
import Postmaster.Base

-- * Static Buffer I\/O

type ReadHandle  = Handle
type WriteHandle = Handle

-- |Run the given computation with an initialized, empty
-- 'Buffer'. The buffer is gone when the computation
-- returns.

withBuffer :: Capacity -> (Buffer -> IO a) -> IO a
withBuffer 0 = fail "BlockIO.withBuffer with size 0 doesn't make sense"
withBuffer n = bracket cons dest
  where
  cons = mallocArray (fromIntegral n) >>= \p -> return (Buf n p 0)
  dest = \(Buf _ p _) -> free p

-- |Drop the first @n <= size@ octets from the buffer.

flush :: ByteCount -> Buffer -> IO Buffer
flush 0 buf               = return buf
flush n (Buf cap ptr len) = assert (n <= len) $ do
  let ptr' = ptr `plusPtr` (fromIntegral n)
      len' = (fromIntegral len) - (fromIntegral n)
  when (len' > 0) (copyArray ptr ptr' len')
  return (Buf cap ptr (fromIntegral len'))

-- |Timeouts are represented in microseconds.

type Timeout = Int

-- |If there is space, read and append more octets; then
-- return the modified buffer. In case of 'hIsEOF',
-- 'Nothing' is returned. If the buffer is full already,
-- 'throw' a 'BufferOverflow' exception. When the timeout
-- exceeds, 'ReadTimeout' is thrown.

slurp :: Timeout -> ReadHandle -> Buffer -> IO (Maybe Buffer)
slurp to h b@(Buf cap ptr len) = do
  when (cap <= len) (throw (BufferOverflow h b))
  timeout to (handleEOF wrap) >>=
    maybe (throw (ReadTimeout to h b)) return
  where
  wrap = do let ptr' = ptr `plusPtr` (fromIntegral len)
                n    = cap - len
            rc <- hGetBufNonBlocking h ptr' (fromIntegral n)
            if rc > 0
               then return (Buf cap ptr (len + (fromIntegral rc)))
               else hWaitForInput h (-1) >> wrap

-- * BlockHandler and I\/O Driver

-- |A callback function suitable for use with 'runLoop'
-- takes a buffer and a state, then returns a modified
-- buffer and a modified state. Usually the callback will
-- use 'slurp' to remove data it has processed already.

type BlockHandler st = Buffer -> st -> IO (Buffer, st)

-- |Our main I\/O driver.

runLoopNB
  :: (st -> Timeout)                -- ^ user state provides timeout
  -> (SomeException -> st -> IO st) -- ^ user provides I\/O error handler
  -> ReadHandle                     -- ^ the input source
  -> Capacity                       -- ^ I\/O buffer size
  -> BlockHandler st                -- ^ callback
  -> st                             -- ^ initial callback state
  -> IO st                          -- ^ return final callback state
runLoopNB mkTO errH hIn cap f initST = withBuffer cap (flip ioloop $ initST)
  where
  ioloop buf st = buf `seq` st `seq`
    handle (\e -> errH e st) $ do
      rc <- slurp (mkTO st) hIn buf
      case rc of
        Nothing   -> return st
        Just buf' -> f buf' st >>= uncurry ioloop

-- |A variant which won't time out and will just 'throw' all
-- exceptions.

runLoop :: ReadHandle -> Capacity -> BlockHandler st -> st -> IO st
runLoop = runLoopNB (const (-1)) (\e _ -> throw e)

-- * Handler Combinators

-- |Signal how many bytes have been consumed from the
-- /front/ of the list; these octets will be dropped.

type StreamHandler st = [Word8] -> st -> IO (ByteCount, st)

handleStream :: StreamHandler st -> BlockHandler st
handleStream f buf@(Buf _ ptr len) st = do
  (i, st') <- peekArray (fromIntegral len) ptr >>= (flip f) st
  buf' <- flush i buf
  return (buf', st')

-- * I\/O Exceptions

-- |Thrown by 'slurp'.

data BufferOverflow = BufferOverflow ReadHandle Buffer
                    deriving (Show, Typeable)

instance Exception BufferOverflow where

-- |Thrown by 'slurp'.

data ReadTimeout    = ReadTimeout Timeout ReadHandle Buffer
                    deriving (Show, Typeable)

instance Exception ReadTimeout where

-- * Internal Helper Functions

-- |Return 'Nothing' if the given computation throws an
-- 'isEOFError' exception. Used by 'slurp'.

handleEOF :: IO a -> IO (Maybe a)
handleEOF f =
  catchJust fromException
    (fmap Just f)
    (\e -> if isEOFError e then return Nothing else ioError e)

-- |Our version of C's @strstr(3)@.

strstr :: [Word8] -> [Word8] -> Maybe Int
strstr tok = strstr' 0
  where
  strstr'  _     []       = Nothing
  strstr' pos ls@(_:xs)
    | tok `isPrefixOf` ls = Just (pos + length tok)
    | otherwise           = strstr' (pos + 1) xs

-- |Split a list by some delimiter. Will soon be provided by
-- "Data.List".

splitList :: Eq a => [a] -> [a] -> [[a]]
splitList d' l' =
  unfoldr (\x -> if (null x) then Nothing else Just $ nextToken d' [] (snd $ splitAt (length d') x)) (d'++l')
  where nextToken _ r [] = (r, [])
        nextToken d r l@(h:t) | (d `isPrefixOf` l) = (r, l)
                              | otherwise = nextToken d (r++[h]) t

-- * Socket Handlers

type SocketHandler = (Socket,SockAddr) -> IO ()

-- |Creates a listening socket for the given port, then
-- calls 'acceptor' to start the given computation for every
-- incoming connection.

listener :: PortID -> SocketHandler -> IO ()
listener p h = bracket (listenOn p) (sClose) (acceptor h)

-- |Given a listening socket, this function will loop
-- forever 'accept'ing incoming connections. For each
-- connection a 'SocketHandler' thread is forked.

acceptor :: SocketHandler -> Socket -> IO ()
acceptor h ls = do
  bracketOnError
    (accept ls)
    (sClose . fst)
    (\peer@(s,_) -> fork $ h peer `finally` sClose s)
  acceptor h ls
  where
  fork f = forkIO f >> return ()

type LazyHandler = (Handle, Maybe SockAddr) -> IO ()

handleLazy :: IOMode -> LazyHandler -> SocketHandler
handleLazy m f (s,sa) =
  bracket (socketToHandle s m) (hClose) (\h -> f (h, Just sa))


-- * Non-blocking I\/O

-- |The exception we throw when writes time out.
-- 'ReadTimeout' is throw by "System.IO.Driver".

data WriteTimeout = WriteTimeout Timeout
                  deriving (Typeable, Show)

instance Exception WriteTimeout where

setReadTimeout :: Timeout -> Smtpd ()
setReadTimeout = local . setVar (mkVar "ReadTimeout")

getReadTimeout :: Smtpd Timeout
getReadTimeout = local $ getVarDef (mkVar "ReadTimeout") (90 * 1000000)

setWriteTimeout :: Timeout -> Smtpd ()
setWriteTimeout = local . setVar (mkVar "WriteTimeout")

getWriteTimeout :: Smtpd Timeout
getWriteTimeout = local $ getVarDef (mkVar "WriteTimeout") (90 * 1000000)

safeWrite :: IO a -> Smtpd a
safeWrite f = do
  to <- getWriteTimeout
  liftIO $ timeout to f >>= maybe (throw (WriteTimeout to)) return

safeReply :: WriteHandle -> SmtpReply -> Smtpd ()
safeReply hOut r = safeWrite (hPutStr hOut (show r))

safeFlush :: WriteHandle -> Smtpd ()
safeFlush hOut = safeWrite (hFlush hOut)
