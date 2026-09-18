module AtomicsExtraTest where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (replicateM_, unless)
import Data.Atomics (casIORef, peekTicket, readForCAS)
import Data.Atomics.Extra (atomicModifyIORefCAS'_)
import Data.Either (isLeft, isRight)
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import Test.Concurrent (concurrentlyBounded)
import Test.Hspec

spec_atomicModifyIORefCAS' :: Spec
spec_atomicModifyIORefCAS' = describe "atomicModifyIORefCAS'_" do
  it "retries under contention without losing any concurrent increments" do
    ref <- newIORef (0 :: Int)
    concurrentlyBounded (replicate 8 (replicateM_ 5_000 (atomicModifyIORefCAS'_ ref (+ 1))))
    result <- readIORef ref
    result `shouldBe` 40_000

  it "raises at the call site and leaves the previous value intact, rather than poisoning the ref" do
    ref <- newIORef (10 :: Int)
    outcome <- try (atomicModifyIORefCAS'_ ref (const (error "boom"))) :: IO (Either SomeException ())
    outcome `shouldSatisfy` isLeft
    readIORef ref `shouldReturn` 10

  it "does not fall into the historical bug a reconstruction of it does under the same forced contention" do
    -- An unconditionally-diverging update throws on attempt 1 regardless of
    -- contention, so it can't distinguish old vs. new behavior directly.
    --
    -- This reconstructs 'Data.Atomics.atomicModifyIORefCAS_''s exact
    -- structure with a small budget instead of its hardcoded 30, forcing
    -- exactly that many CAS losses via a real competing write between each
    -- evaluate and compare-and-swap.
    let budget = 3 :: Int
        poison x = if x >= budget then error "boom" :: Int else x
        reconstructedFallback ref interfere fn = readForCAS ref >>= go budget
          where
            go 0 _ = atomicModifyIORef ref (\old -> (fn old, ()))
            go tries tick = do
              new <- evaluate (fn (peekTicket tick))
              _ <- interfere
              (success, tick') <- casIORef ref tick new
              unless success (go (tries - 1) tick')

    buggyRef <- newIORef (0 :: Int)
    let interfereBuggy = atomicModifyIORefCAS'_ buggyRef (+ 1) -- a genuine, unrelated, successful write
    callOutcome <- try (reconstructedFallback buggyRef interfereBuggy poison) :: IO (Either SomeException ())
    callOutcome `shouldSatisfy` isRight -- the call itself returns normally: the bug
    readOutcome <- try (readIORef buggyRef >>= evaluate) :: IO (Either SomeException Int)
    readOutcome `shouldSatisfy` isLeft -- ...but a later, unrelated read throws instead

    -- Our fix, put through the exact same forced-contention setup:
    fixedRef <- newIORef (0 :: Int)
    replicateM_ budget (atomicModifyIORefCAS'_ fixedRef (+ 1)) -- same three genuine writes
    fixedOutcome <- try (atomicModifyIORefCAS'_ fixedRef poison) :: IO (Either SomeException ())
    fixedOutcome `shouldSatisfy` isLeft -- raises at the call site instead of deferring
    readIORef fixedRef `shouldReturn` budget -- and never installs the poison
