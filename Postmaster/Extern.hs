{- |
   Module      :  Postmaster.Extern
   Copyright   :  (c) 2005-02-05 by Peter Simons
   License     :  GPL2

   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  Haskell 2-pre
 -}

module Postmaster.Extern where

import Prelude hiding ( catch )
import System.IO
import System.Exit ( ExitCode(..) )
import System.Process
import Control.Exception
import Control.Concurrent
import Control.Monad.RWS hiding ( local )
import Child ( timeout )

-- * TODO: All crap! Has to go away.

type ExternHandle = MVar (Handle, Handle, Handle, ProcessHandle)

-- |Run an external process and store its handle in an
-- 'MVar' with a finalizer attached to it that will close
-- the handles and kill the process when the MVar falls out
-- of scope. The process is run with \"@\/@\" as current
-- directory and an /empty/ enviroment. (This may change.)

extern :: FilePath -> [String] -> IO ExternHandle
extern path args = do
  r <- runInteractiveProcess path args (Just "/") (Just [])
  mv <- newMVar r
  addMVarFinalizer mv (catch (cleanup r) (const (return ())))
  let (hin,_,_,_) = r
  hSetBuffering hin NoBuffering
  return mv
    where
    cleanup (hin, hout, herr, pid) = do
      terminateProcess pid
      hClose hin >> hClose hout >> hClose herr
      safeWaitForProcess pid
      return ()

-- |Wait 30 seconds max. If the process hasn't terminated by
-- then, throw an exception. If the child process has been
-- terminated by a signal, return @ExitFailure 137@. This is
-- a kludge. So it will probably be in here forever.

safeWaitForProcess :: ProcessHandle -> IO ExitCode
safeWaitForProcess pid =
  timeout maxwait loop >>= maybe badluck return
    where
    loop    = catch loop' (\_ -> return (ExitFailure 137))
    loop'   = wait >> getProcessExitCode pid >>= maybe loop' return
    wait    = threadDelay 1000000 -- 1 second
    maxwait = 30000000            -- 30 seconds
    badluck = fail "timeout while waiting for external process"


-- ----- Configure Emacs -----
--
-- Local Variables: ***
-- haskell-ghci-program-args: ( "-ladns" "-lcrypto" ) ***
-- End: ***
