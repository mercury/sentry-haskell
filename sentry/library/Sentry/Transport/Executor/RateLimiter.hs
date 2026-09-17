-- | Rate-limit bookkeeping: which data categories cannot be sent yet, and
-- until when.
--
-- Deadlines arrive as 'Delivery.RateLimit' values, which the sender's own
-- module derives from whatever its protocol said.
--
-- Designed to be imported qualified:
--
-- > import Sentry.Transport.Executor.RateLimiter qualified as RateLimiter
-- >
-- > -- Merge what a response announced, never shortening an existing deadline.
-- > let limiter' = RateLimiter.apply limiter outcome.rateLimits
-- >
-- > -- Send only what is currently permitted.
-- > let filtered = RateLimiter.filterEnvelope limiter' now envelope
-- > for_ filtered.kept sendEnvelope
module Sentry.Transport.Executor.RateLimiter
  ( -- * Rate Limiter
    RateLimiter (..),
    new,

    -- * Updating Rate Limits
    apply,

    -- * Querying Rate Limits
    isDisabledUntil,
    isDisabledFor,
    isEnabled,

    -- * Filtering payloads
    FilteredEnvelope (..),
    filterEnvelope,
  )
where

import Data.Function ((&))
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Patrol qualified
import Patrol.Type.DataCategory (DataCategory)
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Items qualified as Patrol.Items
import Sentry.ClientReport (categoryFromItem)
import Sentry.Transport.Delivery qualified as Delivery

-- | Active rate limits, per data category.
--
-- 'global' is the catch-all deadline, applied when a restriction covers every
-- category rather than naming one; 'categories' holds the per-category
-- deadlines.
--
-- This is a pure value that gets threaded through the transport worker loop,
-- use 'apply' to merge in newly announced deadlines.
type RateLimiter :: Type
data RateLimiter = RateLimiter
  { global :: Maybe UTCTime,
    categories :: Map DataCategory UTCTime
  }
  deriving stock (Show)

-- | Create a new rate limiter with no active limits.
new :: RateLimiter
new = RateLimiter{global = Nothing, categories = Map.empty}

-- | Apply explicit limits without shortening any existing deadline.
apply :: RateLimiter -> [Delivery.RateLimit] -> RateLimiter
apply = List.foldl' \rl limit -> case limit.scope of
  Delivery.AllCategories -> rl{global = Just (maybe limit.expiresAt (max limit.expiresAt) rl.global)}
  Delivery.Category category -> rl{categories = Map.insertWith max category limit.expiresAt rl.categories}

-- | Check when a given category is rate limited until.
--
-- 'Nothing' checks only the global limit.  'Just cat' folds both the
-- per-category limit and the global limit, returning the later of the two.
--
-- Returns @Just utcTime@ if rate limited, where @utcTime@ is the time at
-- which rate limiting expires; returns @Nothing@ if not rate limited.
isDisabledUntil :: RateLimiter -> Maybe DataCategory -> Maybe UTCTime
isDisabledUntil rl Nothing = rl.global
isDisabledUntil rl (Just category) =
  case (rl.global, Map.lookup category rl.categories) of
    (Nothing, mc) -> mc
    (mg, Nothing) -> mg
    (Just g, Just c) -> Just (max g c)

-- | Check how many seconds a given category is disabled for.
--
-- Returns @Just duration@ if rate limited, where @duration@ is the number of
-- seconds until rate limiting expires with respect to the provided 'UTCTime'.
--
-- Returns @Nothing@ if not rate limited.
isDisabledFor :: UTCTime -> Maybe DataCategory -> RateLimiter -> Maybe NominalDiffTime
isDisabledFor now m rl = checkExpiry =<< isDisabledUntil rl m
  where
    checkExpiry expiresAt =
      if now < expiresAt
        then Just $ expiresAt `diffUTCTime` now
        else Nothing
{-# INLINEABLE isDisabledFor #-}

-- | Check if a category is currently allowed (not rate limited).
isEnabled :: UTCTime -> Maybe DataCategory -> RateLimiter -> Bool
isEnabled now m rl = isNothing $ isDisabledFor now m rl
{-# INLINEABLE isEnabled #-}

-- | The result of filtering an envelope against the current rate limits.
type FilteredEnvelope :: Type
data FilteredEnvelope = FilteredEnvelope
  { -- | Rateable items removed because their category is currently rate
    -- limited. Items with no rate-limit category (see 'categoryFromItem') are
    -- never listed, since they cannot be charged to a client report.
    dropped :: [Patrol.Item],
    -- | The envelope restricted to the items that may still be sent, or
    -- 'Nothing' when every item was filtered out (including when a global rate
    -- limit applies).
    kept :: Maybe Patrol.Envelope
  }

-- | Filter envelope items based on current rate limits.
--
-- Items whose categories are currently rate limited are removed and surfaced
-- in 'dropped' (so callers can account for them); the rest are returned in
-- 'kept'.
--
-- Items with no associated category (e.g. 'Patrol.Item.Raw',
-- 'Patrol.Item.ClientReport') are subject only to the global rate limit.
filterEnvelope :: RateLimiter -> UTCTime -> Patrol.Envelope -> FilteredEnvelope
filterEnvelope rl now envelope = case envelope.items of
  Patrol.Items.Raw _ ->
    -- Raw payloads are subject only to the global limit, and carry no per-item
    -- categories, so nothing can be attributed to a client report either way.
    if isEnabled now Nothing rl
      then FilteredEnvelope{dropped = [], kept = Just envelope}
      else FilteredEnvelope{dropped = [], kept = Nothing}
  Patrol.Items.EnvelopeItems items ->
    let (keptItems, droppedItems) =
          items & List.partition \item -> isEnabled now (categoryFromItem item) rl
     in FilteredEnvelope
          { dropped = droppedItems,
            kept = case keptItems of
              [] -> Nothing
              remaining ->
                Just envelope{Patrol.Envelope.items = Patrol.Items.EnvelopeItems remaining}
          }
{-# INLINEABLE filterEnvelope #-}
