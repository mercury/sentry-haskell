-- | Client-side event-loss accounting.
--
-- This module provides two things:
--
--   * 'DiscardReason': a typed vocabulary for every point in the SDK where an
--     event can be dropped, used both to populate client reports on the wire
--     and to emit human-readable debug log lines.
--
--   * 'ClientReports': a thread-safe accumulator used by transports to record
--     event drops (with 'record') and drain the queue (with 'takePending')
--     to build a 'Patrol.Type.ClientReport.ClientReport' that can be appended
--     to an outgoing envelope.
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
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Atomics (atomicModifyIORefCAS_)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
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
  deriving stock (Eq, Ord, Show)

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
  { discarded :: IORef (Map (DiscardReason, DataCategory) Int),
    lastSent :: IORef UTCTime
  }

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
  discarded <- newIORef Map.empty
  lastSent <- newIORef now
  pure ClientReports{discarded, lastSent}

-- | Record @n@ discarded events for the given reason and category.
--
-- The @(reason, category)@ keyspace is bounded by the product of the closed
-- 'DiscardReason' and 'DataCategory' vocabularies, so the accumulator needs no
-- cardinality cap.
record :: ClientReports -> DiscardReason -> DataCategory -> Int -> IO ()
record cr reason category n
  | n <= 0 = pure ()
  | otherwise = atomicModifyIORefCAS_ cr.discarded (Map.insertWith (+) (reason, category) n)

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
takePending cr now force = runMaybeT $ do
  lastSent <- lift $ readIORef cr.lastSent
  guard (force || diffUTCTime now lastSent >= piggybackInterval)
  m <- lift $ atomicModifyIORef' cr.discarded (Map.empty,)
  guard (not $ Map.null m)
  lift $ writeIORef cr.lastSent now
  pure
    Patrol.ClientReport.ClientReport
      { Patrol.ClientReport.timestamp = Just now,
        Patrol.ClientReport.discardedEvents =
          [ Patrol.DiscardedEvent.DiscardedEvent
              { Patrol.DiscardedEvent.reason = reasonText reason,
                Patrol.DiscardedEvent.category = category,
                Patrol.DiscardedEvent.quantity = quantity
              }
          | ((reason, category), quantity) <- Map.toList m
          ]
      }

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
