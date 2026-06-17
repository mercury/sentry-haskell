-- | End-to-end profiling harness for the HTTP transports.
--
-- Pushes a fixed number of envelopes through either the synchronous or the
-- asynchronous HTTP transport against a @kent-server@ backend, then flushes and
-- shuts the transport down.
--
-- It is a single deterministic pass so it can be driven by both @hyperfine@ and
-- under the profiling RTS (@+RTS -p\/-hc\/-l@).
--
-- Usage:
--
-- > sentry-profile <sync|async> [count] [queueSize] [payloadBytes]
--
-- Backend selection:
--
--   * If @SENTRY_PROFILE_KENT_PORT@ is set, the harness talks to an
--     already-running kent on that port and does /not/ manage its lifecycle —
--     this is what @hyperfine@ drives, so kent boot\/teardown stays out of the
--     timed region.
--   * Otherwise it spawns its own kent via 'Kent.withKent' and verifies
--     delivery at the end.
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Kind (Type)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Network.HTTP.Client qualified as HttpClient
import Patrol qualified
import Sentry.TestKit.Gen qualified as Gen
import Sentry.TestKit.Kent (KentHandle (..))
import Sentry.TestKit.Kent qualified as Kent
import Sentry.Transport qualified as Transport
import Sentry.Transport.HTTP.Async qualified as AsyncHttp
import Sentry.Transport.HTTP.Sync qualified as SyncHttp
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | Which transport to exercise.
type Mode :: Type
data Mode = Sync | Async

-- | Tallied outcomes from a run.
type Counts :: Type
data Counts = Counts
  { -- | Envelopes accepted by the transport (every event, barring shutdown).
    processed :: !Int,
    -- | Backpressure events: a 'Transport.SendFailed_QueueFull' that we paused
    -- on and retried (so the event still went through).
    retries :: !Int,
    -- | Events abandoned because the transport reported shutdown mid-run.
    shutdownDropped :: !Int
  }

-- | Parsed harness configuration.
type Config :: Type
data Config = Config
  { mode :: Mode,
    count :: Int,
    queueSize :: Int,
    payloadBytes :: Int
  }

defaultCount :: Int
defaultCount = 5000

main :: IO ()
main = do
  cfg <- parseArgs =<< getArgs
  externalPort <- (>>= readMaybe) <$> lookupEnv "SENTRY_PROFILE_KENT_PORT"
  case externalPort of
    Just port -> do
      manager <- HttpClient.newManager HttpClient.defaultManagerSettings
      run cfg KentHandle{port, manager} False
    Nothing ->
      Kent.withKent \kent -> run cfg kent True

-- | Construct the configured transport, drive the run, and report.
run :: Config -> KentHandle -> Bool -> IO ()
run cfg kent verify = do
  let dsn = Kent.dsnFor kent "1"
      envelope = mkEnvelope cfg dsn
  putStrLn $
    "sentry-profile: "
      <> modeLabel cfg.mode
      <> " count="
      <> show cfg.count
      <> (case cfg.mode of Async -> " queue=" <> show cfg.queueSize; Sync -> "")
      <> " payloadBytes="
      <> show cfg.payloadBytes
  start <- getCurrentTime
  counts <- case cfg.mode of
    Sync -> do
      transport <- SyncHttp.new False kent.manager Nothing dsn
      counts <- drive transport cfg.count envelope
      -- Generous timeouts: flush must drain the whole queue through real HTTP.
      _ <- Transport.flush transport 300
      _ <- Transport.shutdown transport 300
      pure counts
    Async -> do
      transport <- AsyncHttp.new cfg.queueSize False kent.manager Nothing dsn
      counts <- drive transport cfg.count envelope
      -- Generous timeouts: flush must drain the whole queue through real HTTP.
      _ <- Transport.flush transport 300
      _ <- Transport.shutdown transport 300
      pure counts
  end <- getCurrentTime
  let elapsed = realToFrac (diffUTCTime end start) :: Double
  report counts elapsed
  when verify do
    -- kent retains only a bounded, most-recent window in memory, so this is a
    -- sanity check that delivery happened.
    retained <- length <$> Kent.listEvents kent
    putStrLn $ "  kent retained (bounded window): " <> show retained

-- | Push @total@ envelopes all the way through the transport, applying
-- backpressure on a full queue so every event traverses the worker rather than
-- being dropped.
drive :: (Transport.Transport t) => t -> Int -> Patrol.Envelope -> IO Counts
drive transport total envelope = go total (Counts 0 0 0)
  where
    go 0 !counts = pure counts
    go k !counts =
      Transport.send transport envelope >>= \case
        Transport.SendProcessed -> go (k - 1) counts{processed = counts.processed + 1}
        -- Queue full: back off briefly (rather than hot-spinning, which would
        -- dominate the profile) and retry the same envelope; k is not
        -- decremented, so the event is delivered rather than dropped.
        Transport.SendFailed_QueueFull -> threadDelay 100 *> go k counts{retries = counts.retries + 1}
        -- Transport shut down underneath us: abandon the remaining k events.
        Transport.SendFailed_Shutdown -> pure counts{shutdownDropped = counts.shutdownDropped + k}

report :: Counts -> Double -> IO ()
report counts elapsed = do
  printf "  elapsed: %.3fs\n" elapsed
  let throughput = if elapsed > 0 then fromIntegral counts.processed / elapsed else 0 :: Double
  printf "  processed: %d  (%.0f/s)\n" counts.processed throughput
  printf "  queue-full retries: %d\n" counts.retries
  printf "  shutdown drops: %d\n" counts.shutdownDropped

mkEnvelope :: Config -> Patrol.Dsn -> Patrol.Envelope
mkEnvelope cfg dsn
  | cfg.payloadBytes > 0 = Gen.messageEnvelope dsn cfg.payloadBytes
  | otherwise = Gen.sampleEnvelope dsn

modeLabel :: Mode -> String
modeLabel = \case
  Sync -> "sync"
  Async -> "async"

parseArgs :: [String] -> IO Config
parseArgs = \case
  [] -> die usage
  (m : rest) -> do
    mode <- case m of
      "sync" -> pure Sync
      "async" -> pure Async
      _ -> die usage
    let count = positional 0 defaultCount rest
    pure
      Config
        { mode,
          count,
          -- Default the queue to the event count so the default run applies no
          -- backpressure; pass a smaller value explicitly to study a bounded
          -- queue under load.
          queueSize = positional 1 count rest,
          payloadBytes = positional 2 0 rest
        }
  where
    positional i fallback xs = maybe fallback id (readMaybe =<< nth i xs)
    nth i xs = case drop i xs of
      (x : _) -> Just x
      [] -> Nothing

usage :: String
usage =
  unlines
    [ "usage: sentry-profile <sync|async> [count] [queueSize] [payloadBytes]",
      "",
      "  count         number of envelopes to send (default 5000)",
      "  queueSize     async bounded-queue capacity (async only; default: count,",
      "                i.e. no backpressure)",
      "  payloadBytes  if > 0, send a message event of this size instead of the",
      "                default exception event (default 0)",
      "",
      "  env SENTRY_PROFILE_KENT_PORT  talk to an already-running kent on this",
      "                                port instead of spawning one"
    ]
