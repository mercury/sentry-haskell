module Main where

import Control.Monad (replicateM_, void)
import Patrol qualified
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Transport qualified as Transport
import Sentry.Transport.Executor.RateLimiter (RateLimiter)
import Sentry.Transport.Executor.Async (AsyncExecutor)
import Sentry.Transport.Executor.Async qualified as AsyncExecutor
import Test.Tasty (TestTree, withResource)
import Test.Tasty.Bench qualified as Bench
import UnliftIO.Exception (SomeException (..))

main :: IO ()
main = do
  Bench.defaultMain
    [ Bench.bgroup "no-op executor" benchNoopExecutor
    ]

benchNoopExecutor :: [Bench.Benchmark]
benchNoopExecutor =
  [ withNoopExecutor \acquire -> Bench.bench "100" $ Bench.nfIO $ sendNTimes 100 acquire,
    withNoopExecutor \acquire -> Bench.bench "10_000" $ Bench.nfIO $ sendNTimes 10_000 acquire,
    withNoopExecutor \acquire -> Bench.bench "1_000_000" $ Bench.nfIO $ sendNTimes 1_000_000 acquire
  ]

withNoopExecutor :: (IO (AsyncExecutor, Patrol.Envelope) -> TestTree) -> TestTree
withNoopExecutor = withResource
  ((,) <$> AsyncExecutor.new AsyncExecutor.defaultQueueSize noopSend <*> mkTestEnvelope)
  (\(executor, _) -> void $ Transport.shutdown executor 1)

noopSend :: Patrol.Envelope -> RateLimiter -> IO RateLimiter
noopSend (!_) (!rl) = pure rl

sendNTimes :: Int -> IO (AsyncExecutor, Patrol.Envelope) -> IO ()
sendNTimes times acquire = do
  (executor, envelope) <- acquire
  replicateM_ times (void $ Transport.send executor envelope)

-- | A valid 'Patrol.Type.Envelope.Envelope', derived from the 'testEvent' and
-- 'testDsn' mocks.
mkTestEnvelope :: IO Patrol.Envelope
mkTestEnvelope = Patrol.Envelope.fromEvent testDsn <$> mkTestEvent

-- | A valid 'Patrol.Type.Event.Event' mock.
mkTestEvent :: IO Patrol.Event
mkTestEvent = Patrol.Event.fromSomeException $ SomeException $ userError "boom"

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
