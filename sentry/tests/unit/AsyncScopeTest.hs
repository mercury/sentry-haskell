-- | Exercises the scope-propagating spawn helpers in "Sentry.Async", driven
-- through the "Sentry" facade.
--
-- Each case installs a real isolation and current scope with
-- 'Sentry.withIsolationScope' and 'Sentry.withScope' before spawning, so the
-- spawned thread has genuine ambient scope to inherit rather than an empty
-- one. 'Sentry.withAsyncScoped' and 'Sentry.forConcurrentlyScoped' are
-- one-line compositions of functions already covered here, so they don't get
-- separate cases.
module AsyncScopeTest where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVar, readTVar)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Sentry qualified as Sentry
import Sentry.Scope.Operations qualified as ScopeOperations
import Sentry.Test qualified as Test
import Test.Hspec

spec_forkScopedIO :: Spec
spec_forkScopedIO = describe "forkScopedIO" do
  it "inherits both scope layers, clones current independently, and never leaks the clone back" $ Test.withGlobalScope do
    Sentry.withIsolationScope \iso -> Sentry.withScope \cur -> do
      ScopeOperations.setTag iso "isolation" "parent"
      ScopeOperations.setTag cur "current" "parent"
      inherited <- newEmptyMVar
      done <- newEmptyMVar
      _ <- Sentry.forkScopedIO do
        -- Read before this thread writes anything, to demonstrate inheritance.
        beforeWrite <- Sentry.readMergedScope
        putMVar inherited beforeWrite
        childIso <- Sentry.getIsolationScope
        ScopeOperations.setTag childIso "isolation" "from-child"
        childCur <- Sentry.getCurrentScope
        ScopeOperations.setTag childCur "current" "from-child"
        putMVar done ()
      takeMVar done
      beforeWrite <- takeMVar inherited
      beforeWrite.tags `shouldBe` Map.fromList [("isolation", "parent"), ("current", "parent")]
      -- The isolation write is shared by reference and is visible here; the
      -- current write landed on the child's own clone and never leaked back.
      afterJoin <- Sentry.readMergedScope
      afterJoin.tags `shouldBe` Map.fromList [("isolation", "from-child"), ("current", "parent")]

spec_asyncScoped :: Spec
spec_asyncScoped = describe "asyncScoped" do
  it "inherits both scope layers, clones current independently, and returns its result" $ Test.withGlobalScope do
    Sentry.withIsolationScope \iso -> Sentry.withScope \cur -> do
      ScopeOperations.setTag iso "isolation" "parent"
      ScopeOperations.setTag cur "current" "parent"
      handle <- Sentry.asyncScoped do
        beforeWrite <- Sentry.readMergedScope
        childCur <- Sentry.getCurrentScope
        ScopeOperations.setTag childCur "current" "from-child"
        pure beforeWrite
      beforeWrite <- Async.wait handle
      beforeWrite.tags `shouldBe` Map.fromList [("isolation", "parent"), ("current", "parent")]
      afterJoin <- Sentry.readMergedScope
      afterJoin.tags `shouldBe` Map.fromList [("isolation", "parent"), ("current", "parent")]

spec_concurrentBranches :: Spec
spec_concurrentBranches = describe "concurrentlyScoped and mapConcurrentlyScoped" do
  it "gives each branch of concurrentlyScoped an independent current-scope clone" $ Test.withGlobalScope do
    Sentry.withIsolationScope \_ -> Sentry.withScope \_ -> do
      barrier <- atomically $ newTVar 0
      (left, right) <-
        Sentry.concurrentlyScoped
          (observeIndependentClone barrier 2 "left")
          (observeIndependentClone barrier 2 "right")
      left `shouldBe` ("left", Just "left")
      right `shouldBe` ("right", Just "right")

  it "gives each branch of mapConcurrentlyScoped an independent current-scope clone" $ Test.withGlobalScope do
    Sentry.withIsolationScope \_ -> Sentry.withScope \_ -> do
      let labels = ["a", "b", "c", "d"] :: [Text]
      barrier <- atomically $ newTVar 0
      results <- Sentry.mapConcurrentlyScoped (observeIndependentClone barrier (length labels)) labels
      results `shouldBe` [(label, Just label) | label <- labels]

-- | Set a distinct tag on the calling thread's current scope, then block
-- until every concurrent branch sharing the given barrier has done the same
-- before reading the tag back.
--
-- Every write happens before any read, so if the current-scope clones were
-- wrongly aliased across branches (e.g. a bug sharing one clone instead of
-- giving each branch its own), every read would observe whichever branch
-- wrote last instead of its own value.
observeIndependentClone :: TVar Int -> Int -> Text -> IO (Text, Maybe Text)
observeIndependentClone barrier total label = do
  cur <- Sentry.getCurrentScope
  ScopeOperations.setTag cur "current" label
  atomically $ modifyTVar' barrier (+ 1)
  atomically $ readTVar barrier >>= check . (== total)
  snapshot <- ScopeOperations.readScopeRef cur
  pure (label, Map.lookup "current" snapshot.tags)
