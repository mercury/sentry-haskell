-- | Client-side event-loss accounting.
--
-- 'DiscardReason' is a typed vocabulary for every point in the SDK where an event
-- can be dropped, used both to populate client reports on the wire and to emit
-- human-readable debug log lines.
--
-- 'ClientReports' is a thread-safe accumulator used by transports to record
-- dropped events and drain the queue to build a 'Patrol.Type.ClientReport.ClientReport'
-- that can be appended to an outgoing envelope.
--
-- <https://develop.sentry.dev/sdk/telemetry/client-reports/>
module Sentry.ClientReport
  ( -- * Discard reasons
    DiscardReason (..),
    reasonText,

    -- * Accumulator
    ClientReports,
    new,
    record,
    takePending,

    -- * Envelope drop accounting
    categoryFromItem,
    recordItemDrops,
    recordEnvelopeDrop,
    attach,

    -- * Constants
    piggybackInterval,
  )
where

import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Atomics.Counter (AtomicCounter, incrCounter_, newCounter, readCounter)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Traversable (for)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Patrol qualified
import Patrol.Type.ClientReport qualified as Patrol.ClientReport
import Patrol.Type.DataCategory (DataCategory (..))
import Patrol.Type.DiscardedEvent qualified as Patrol.DiscardedEvent
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Item qualified as Patrol.Item
import Patrol.Type.Items qualified as Patrol.Items

-- | The reason an event was discarded by the SDK.
--
-- Each constructor maps to one of Sentry's registered discard-reason strings
-- via 'reasonText'. The vocabulary is closed (mirroring the official SDKs,
-- e.g. sentry-rust's @#[non_exhaustive]@ @DiscardReason@ enum); 'Other' is the
-- catch-all for drops that do not fit a registered reason, which the backend
-- buckets under @"other"@ anyway.
type DiscardReason :: Type
data DiscardReason
  = BeforeSend
  | EventProcessor
  | InternalSdkError
  | NetworkError
  | QueueOverflow
  | RatelimitBackoff
  | SampleRate
  | SendError
  | Other
  deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | Render a 'DiscardReason' to its Sentry wire string.
reasonText :: DiscardReason -> Text
reasonText = \case
  BeforeSend -> "before_send"
  EventProcessor -> "event_processor"
  InternalSdkError -> "internal_sdk_error"
  NetworkError -> "network_error"
  QueueOverflow -> "queue_overflow"
  RatelimitBackoff -> "ratelimit_backoff"
  SampleRate -> "sample_rate"
  SendError -> "send_error"
  Other -> "other"

-- | A thread-safe accumulator of discarded-event counts.
--
-- The accumulator is opaque: construct it with 'new', write with 'record',
-- and drain with 'takePending'.
type ClientReports :: Type
data ClientReports = ClientReports
  { discarded :: Vector AtomicCounter,
    lastSent :: IORef UTCTime
  }

-- | Every 'DataCategory', in the canonical order used to index 'discarded'.
allCategories :: [DataCategory]
allCategories =
  [ Default,
    Error,
    Transaction,
    Monitor,
    Span,
    LogItem,
    Security,
    Attachment,
    Session,
    Profile,
    ProfileChunk,
    Replay,
    Feedback,
    TraceMetric,
    Internal
  ]

-- | Index of a 'DataCategory' on the category axis of the grid.
--
-- __NOTE__: Must agree with 'allCategories'.
categoryIndex :: DataCategory -> Int
categoryIndex = \case
  Default -> 0
  Error -> 1
  Transaction -> 2
  Monitor -> 3
  Span -> 4
  LogItem -> 5
  Security -> 6
  Attachment -> 7
  Session -> 8
  Profile -> 9
  ProfileChunk -> 10
  Replay -> 11
  Feedback -> 12
  TraceMetric -> 13
  Internal -> 14

-- | Number of 'DataCategory' cells per 'DiscardReason'.
numCategories :: Int
numCategories = length allCategories

-- | Number of 'DiscardReason' rows in the accumulator.
numReasons :: Int
numReasons = fromEnum (maxBound :: DiscardReason) + 1

-- | Flatten a @(reason, category)@ pair to its cell index in 'discarded'.
--
-- Row-major: each reason owns a contiguous run of 'numCategories' cells.
cellIndex :: DiscardReason -> DataCategory -> Int
cellIndex reason category = fromEnum reason * numCategories + categoryIndex category

-- | How long to wait between piggybacked client reports, in seconds.
--
-- 'takePending' respects this interval unless called with @force = True@.
piggybackInterval :: NominalDiffTime
piggybackInterval = 30

-- | Create a new, empty 'ClientReports' accumulator, seeding 'lastSent' to
-- the current time so the first piggybacked report waits a full interval.
new :: IO ClientReports
new = do
  now <- getCurrentTime
  discarded <- Vector.replicateM (numReasons * numCategories) (newCounter 0)
  lastSent <- newIORef now
  pure ClientReports{discarded, lastSent}

-- | Record @n@ discarded events for the given reason and category.
--
-- A single atomic add into the @(reason, category)@ cell: allocation-free, and
-- distinct keys never contend. The keyspace is bounded by the product of the
-- closed 'DiscardReason' and 'DataCategory' vocabularies, so the accumulator
-- needs no cardinality cap.
record :: ClientReports -> DiscardReason -> DataCategory -> Int -> IO ()
record cr reason category n
  | n <= 0 = pure ()
  | otherwise = incrCounter_ n (cr.discarded Vector.! cellIndex reason category)

-- | Snapshot and reset the accumulator, returning a
-- 'Patrol.Type.ClientReport.ClientReport' if there is anything pending.
--
-- Returns 'Nothing' when:
--   * the accumulator is empty, or
--   * fewer than 'piggybackInterval' seconds have elapsed since the last
--     successful drain and @force@ is 'False'.
--
-- When @force@ is 'True' the interval check is skipped, suitable for
-- 'Sentry.Transport.flush' and 'Sentry.Transport.shutdown' paths.
takePending :: ClientReports -> UTCTime -> Bool -> IO (Maybe Patrol.ClientReport.ClientReport)
takePending cr now force = runMaybeT do
  lastSent <- lift $ readIORef cr.lastSent
  guard (force || diffUTCTime now lastSent >= piggybackInterval)
  events <- lift $ drain cr
  guard (not (null events))
  lift $ writeIORef cr.lastSent now
  pure
    Patrol.ClientReport.ClientReport
      { Patrol.ClientReport.timestamp = Just now,
        Patrol.ClientReport.discardedEvents = events
      }

-- | Read and reset every cell in 'discarded', returning one 'DiscardedEvent'
-- per nonzero count.
drain :: ClientReports -> IO [Patrol.DiscardedEvent.DiscardedEvent]
drain cr = do
  counts <- for grid \(reason, category) -> do
    let counter = cr.discarded Vector.! cellIndex reason category
    quantity <- readCounter counter
    incrCounter_ (negate quantity) counter
    pure (reason, category, quantity)
  pure
    [ Patrol.DiscardedEvent.DiscardedEvent
        { Patrol.DiscardedEvent.reason = reasonText reason,
          Patrol.DiscardedEvent.category = category,
          Patrol.DiscardedEvent.quantity = quantity
        }
    | (reason, category, quantity) <- counts,
      quantity /= 0
    ]
  where
    grid :: [(DiscardReason, DataCategory)]
    grid = [(reason, category) | reason <- [minBound ..], category <- allCategories]

-- | Resolve the 'DataCategory' an envelope item is accounted under, if any.
categoryFromItem :: Patrol.Item -> Maybe DataCategory
categoryFromItem = \case
  Patrol.Item.Event _ -> Just Error
  Patrol.Item.ClientReport _ -> Nothing
  Patrol.Item.Raw -> Nothing

-- | Record one drop per rateable item in a list of envelope items.
--
-- Items whose 'categoryFromItem' is 'Nothing' are skipped, so self-reporting
-- items are never counted.
recordItemDrops :: ClientReports -> DiscardReason -> [Patrol.Item] -> IO ()
recordItemDrops cr reason items =
  for_ items \item ->
    for_ (categoryFromItem item) \category ->
      record cr reason category 1

-- | Record one drop per rateable item in an envelope.
--
-- Raw payloads carry no per-item categories and are ignored.
recordEnvelopeDrop :: ClientReports -> DiscardReason -> Patrol.Envelope -> IO ()
recordEnvelopeDrop cr reason envelope = case envelope.items of
  Patrol.Items.Raw _ -> pure ()
  Patrol.Items.EnvelopeItems items -> recordItemDrops cr reason items

-- | Append a client report as an item on an outgoing envelope (piggybacking).
--
-- Envelopes carrying a raw payload cannot take additional items and are
-- returned unchanged.
attach :: Patrol.ClientReport.ClientReport -> Patrol.Envelope -> Patrol.Envelope
attach report envelope = case envelope.items of
  Patrol.Items.EnvelopeItems items ->
    envelope
      { Patrol.Envelope.items =
          Patrol.Items.EnvelopeItems (items ++ [Patrol.Item.ClientReport report])
      }
  Patrol.Items.Raw _ -> envelope
