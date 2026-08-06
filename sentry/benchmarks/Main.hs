module Main where

import Control.Monad (replicateM_, void)
import Data.Maybe (isJust)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Transport qualified as Transport
import Sentry.Transport.Executor.Async (AsyncExecutor)
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
import Test.Tasty (TestTree, withResource)
import Test.Tasty.Bench qualified as Bench
import UnliftIO.Exception (SomeException (..))

-- | These benchmarks measure the in-process overhead of the executor and rate
-- limiter only, using a no-op send function so no HTTP backend is involved.
-- For end-to-end timing against a real backend, use the @sentry-profile@
-- executable with @hyperfine@.
main :: IO ()
main = do
  Bench.defaultMain
    [ Bench.bgroup "async no-op executor (queue=30)" (benchNoopExecutor 30),
      Bench.bgroup "async no-op executor (queue=1000)" (benchNoopExecutor 1000),
      Bench.bgroup "rate limiter" benchRateLimiter
    ]

-- | Throughput of 'Transport.send' through the async executor with a no-op send
-- function, at a given queue size.
benchNoopExecutor :: Int -> [Bench.Benchmark]
benchNoopExecutor queueSize =
  [ withNoopExecutor queueSize \acquire -> Bench.bench "100" $ Bench.nfIO $ sendNTimes 100 acquire,
    withNoopExecutor queueSize \acquire -> Bench.bench "10_000" $ Bench.nfIO $ sendNTimes 10_000 acquire,
    withNoopExecutor queueSize \acquire -> Bench.bench "1_000_000" $ Bench.nfIO $ sendNTimes 1_000_000 acquire
  ]

-- | Cost of the pure rate-limiter envelope filter on the send path.
benchRateLimiter :: [Bench.Benchmark]
benchRateLimiter =
  [ withTestEnvelope \acquire -> Bench.bench "filterEnvelope (open)" $ Bench.nfIO do
      envelope <- acquire
      let filtered = RateLimiter.filterEnvelope RateLimiter.new fixedTime envelope
      pure $! isJust filtered.kept
  ]

withNoopExecutor :: Int -> (IO (AsyncExecutor, Patrol.Envelope) -> TestTree) -> TestTree
withNoopExecutor queueSize =
  withResource
    ((,) <$> AsyncExecutor.new queueSize Nothing noopSend <*> mkTestEnvelope)
    (\(executor, _) -> void $ Transport.shutdown executor 1)

withTestEnvelope :: (IO Patrol.Envelope -> TestTree) -> TestTree
withTestEnvelope = withResource mkTestEnvelope (const $ pure ())

noopSend :: Patrol.Envelope -> RateLimiter -> IO RateLimiter
noopSend (!_) (!rl) = pure rl

sendNTimes :: Int -> IO (AsyncExecutor, Patrol.Envelope) -> IO ()
sendNTimes times acquire = do
  (executor, envelope) <- acquire
  replicateM_ times (void $ Transport.send executor envelope)

-- | A valid 'Patrol.Type.Envelope.Envelope', derived from the 'mkTestEvent' and
-- 'testDsn' mocks.
mkTestEnvelope :: IO Patrol.Envelope
mkTestEnvelope = Patrol.Envelope.fromEvent testDsn <$> mkTestEvent

-- | A valid 'Patrol.Type.Event.Event' mock.
mkTestEvent :: IO Patrol.Event
mkTestEvent = Patrol.Event.fromSomeException $ SomeException $ userError "boom"

-- | A fixed timestamp so the rate-limiter benchmark stays deterministic.
fixedTime :: UTCTime
fixedTime = UTCTime (fromGregorian 2025 1 1) 0

-- | A valid 'Patrol.Type.Dsn.Dsn' mock.
testDsn :: Patrol.Dsn
testDsn =
  Patrol.Dsn.Dsn
    { Patrol.Dsn.protocol = "a",
      Patrol.Dsn.publicKey = "b",
      Patrol.Dsn.secretKey = "",
      Patrol.Dsn.host = "c",
      Patrol.Dsn.port = Nothing,
      Patrol.Dsn.path = "/",
      Patrol.Dsn.projectId = "d"
    }
