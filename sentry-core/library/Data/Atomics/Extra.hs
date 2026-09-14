-- | Extensions to "Data.Atomics" (from the @atomic-primops@ package).
module Data.Atomics.Extra (atomicModifyIORefCAS'_) where

import Control.Concurrent (yield)
import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.Atomics (casIORef, peekTicket, readForCAS)
import Data.IORef (IORef)

-- | Like 'Data.Atomics.atomicModifyIORefCAS_', but forces the new value to
-- weak head normal form on /every/ compare-and-swap attempt, with no
-- attempt-count-based fallback.
--
-- This variant keeps retrying the compare-and-swap indefinitely, so evaluation
-- always happens strictly before the swap is attempted.
--
-- This is lock-free, not wait-free: a failed attempt only happens because some
-- /other/ thread's write just succeeded, so the system as a whole always makes
-- progress and cannot deadlock, but an individual caller's worst-case retry
-- count is not bounded.
atomicModifyIORefCAS'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORefCAS'_ ref f = loop
  where
    loop = do
      ticket <- readForCAS ref
      new <- evaluate (f (peekTicket ticket))
      (success, _) <- casIORef ref ticket new
      -- reduce cache-line contention under heavy concurrency
      unless success (yield >> loop)
