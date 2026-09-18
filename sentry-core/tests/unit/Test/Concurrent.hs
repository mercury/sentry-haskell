module Test.Concurrent (concurrentlyBounded) where

import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (finally, throwIO)
import Data.Foldable (for_)
import System.Timeout (timeout)
import Test.Hspec (shouldBe)

-- | Start workers together, propagate their exceptions, and bound completion to five seconds.
concurrentlyBounded :: [IO ()] -> IO ()
concurrentlyBounded actions = do
  start <- newEmptyMVar
  workers <-
    traverse
      ( \action -> do
          done <- newEmptyMVar
          tid <- forkFinally (readMVar start >> action) (putMVar done)
          pure (tid, done)
      )
      actions
  let wait = do
        putMVar start ()
        for_ workers \(_, done) -> takeMVar done >>= either throwIO pure
  completed <- timeout 5_000_000 wait `finally` for_ workers (killThread . fst)
  completed `shouldBe` Just ()
