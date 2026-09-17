module ScopePropagationTest where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Scope.Operations (ScopeData (..), ScopeType (..))
import Sentry.Scope.Operations qualified as Scope
import Sentry.Test qualified as Test
import Test.Hspec

-- | These exercise 'Scope.propagateScope' directly, composed with plain
-- 'forkIO' — 'Sentry.Async.forkScopedIO' itself lives in the @sentry@
-- package, one layer up, and isn't available to @sentry-core@.
--
-- Every case installs its own isolation and current scopes via
-- 'Scope.IO.withIsolationScope' and 'Scope.IO.withScope' so that a real
-- pre-existing scope is in play, then forks (or, in one case, doesn't fork)
-- the deferred action 'Scope.propagateScope' hands back. Children signal
-- completion through an 'Control.Concurrent.MVar' rather than asserting
-- directly, since a failed assertion on a forked thread only prints instead
-- of failing the test.
spec_propagateScope :: Spec
spec_propagateScope = describe "propagateScope" do
  it "gives a forked child both the isolation and a cloned current scope" $ Test.withGlobalScope do
    Scope.IO.withIsolationScope \isoScope -> Scope.IO.withScope \curScope -> do
      Scope.setTag isoScope "isolation" "parent"
      Scope.setTag curScope "current" "parent"
      done <- newEmptyMVar
      seen <- newEmptyMVar
      deferred <- Scope.propagateScope do
        merged <- Scope.readMergedScope
        putMVar seen merged
      void $ forkIO (deferred >> putMVar done ())
      takeMVar done
      merged <- takeMVar seen
      merged.tags `shouldBe` Map.fromList [("isolation", "parent"), ("current", "parent")]

  it "does not leak a child's current-scope write back to the parent" $ Test.withGlobalScope do
    Scope.IO.withIsolationScope \_ -> Scope.IO.withScope \_ -> do
      done <- newEmptyMVar
      deferred <- Scope.propagateScope do
        ctx <- ThreadLocal.getContext
        case Scope.lookupCurrent ctx of
          Just childCurrent -> Scope.setTag childCurrent "child" "discarded"
          Nothing -> expectationFailure "expected a current scope in the child"
      void $ forkIO (deferred >> putMVar done ())
      takeMVar done
      merged <- Scope.readMergedScope
      merged.tags `shouldBe` Map.empty

  it "shares a child's isolation-scope write with the parent" $ Test.withGlobalScope do
    Scope.IO.withIsolationScope \_ -> Scope.IO.withScope \_ -> do
      done <- newEmptyMVar
      deferred <- Scope.propagateScope do
        childIso <- Scope.getIsolationScope
        Scope.setTag childIso "isolation" "from-child"
      void $ forkIO (deferred >> putMVar done ())
      takeMVar done
      merged <- Scope.readMergedScope
      merged.tags `shouldBe` Map.singleton "isolation" "from-child"

  it "leaves a parent with no current scope without one, and creates an empty current scope for the child" $
    Test.withGlobalScope do
      -- No 'Scope.IO.withIsolationScope'/'Scope.IO.withScope' bracket here:
      -- both install a current scope as well as isolation, and this case
      -- means to exercise the calling thread having no current scope at all.
      ctxBefore <- ThreadLocal.getContext
      Scope.lookupCurrent ctxBefore `shouldSatisfy` isNothing
      done <- newEmptyMVar
      seenType <- newEmptyMVar
      deferred <- Scope.propagateScope do
        ctx <- ThreadLocal.getContext
        case Scope.lookupCurrent ctx of
          Just childCurrent -> do
            snapshot <- Scope.readScopeRef childCurrent
            putMVar seenType snapshot.type_
          Nothing -> putMVar seenType Nothing
      void $ forkIO (deferred >> putMVar done ())
      takeMVar done
      takeMVar seenType `shouldReturn` Just Current
      ctxAfter <- ThreadLocal.getContext
      Scope.lookupCurrent ctxAfter `shouldSatisfy` isNothing

  it "is safe to run on the calling thread, and always restores the current-scope key afterward" $
    Test.withGlobalScope do
      Scope.IO.withIsolationScope \_ -> Scope.IO.withScope \parentCurrent -> do
        Scope.setTag parentCurrent "current" "parent-marker"
        childTag <- newEmptyMVar
        deferred <- Scope.propagateScope do
          ctx <- ThreadLocal.getContext
          case Scope.lookupCurrent ctx of
            Just childCurrent -> do
              snapshot <- Scope.readScopeRef childCurrent
              putMVar childTag (Map.lookup "current" snapshot.tags)
            Nothing -> putMVar childTag Nothing
        -- Run the deferred action directly, on the calling thread, rather than
        -- forking it.
        deferred
        -- The clone saw the parent's tag at capture time.
        takeMVar childTag `shouldReturn` Just "parent-marker"
        -- The calling thread's own current scope, with its own tag, is back
        -- in place once the deferred action completes.
        restored <- Scope.readScopeRef parentCurrent
        Map.lookup "current" restored.tags `shouldBe` Just "parent-marker"
