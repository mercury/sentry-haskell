-- | What the SDK does about an envelope delivery attempt.
--
-- A sender translates whatever its protocol reported into these types, and the
-- executor acts on them; "Sentry.Transport.HTTP.Delivery" translates between
-- HTTP responses and these types.
--
-- Designed to be imported qualified:
--
-- > import Sentry.ClientReport qualified as ClientReport
-- > import Sentry.Transport.Delivery qualified as Delivery
-- >
-- > sender envelope = do
-- >   ok <- mySend envelope
-- >   pure $ if ok then Delivery.accepted else Delivery.rejected ClientReport.NetworkError
module Sentry.Transport.Delivery
  ( Outcome (..),
    Disposition (..),
    DiscardAccounting (..),
    RateLimit (..),
    RateLimitScope (..),
    discardReason,
    recordOutcome,

    -- * Smart constructors
    accepted,
    acceptedWith,
    rejected,
    throttled,
    allCategoriesUntil,
    categoryUntil,
  ) where

import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Time.Clock (UTCTime)
import Patrol qualified
import Patrol.Type.DataCategory (DataCategory)
import Sentry.ClientReport (ClientReports, DiscardReason)
import Sentry.Discard qualified as Discard

-- | Delivery disposition and explicit limits learned during the attempt.
type Outcome :: Type
data Outcome = Outcome {disposition :: Disposition, rateLimits :: [RateLimit]}
  deriving stock (Eq, Show)

-- | Acceptance transfers responsibility under the sender's own contract.
--
-- It does not guarantee eventual storage by Sentry.
type Disposition :: Type
data Disposition = Accepted | Rejected DiscardAccounting
  deriving stock (Eq, Show)

-- | Whether the SDK should record a rejection in its client reports.
type DiscardAccounting :: Type
data DiscardAccounting = RecordLocally DiscardReason | AccountedUpstream
  deriving stock (Eq, Show)

-- | An absolute restriction deadline for a category or all categories.
type RateLimit :: Type
data RateLimit = RateLimit {scope :: RateLimitScope, expiresAt :: UTCTime}
  deriving stock (Eq, Show)

-- | The scope of an explicit delivery restriction.
type RateLimitScope :: Type
data RateLimitScope = AllCategories | Category DataCategory
  deriving stock (Eq, Show)

-- | The reason an attempt was rejected.
discardReason :: Outcome -> Maybe DiscardReason
discardReason outcome = case outcome.disposition of
  Accepted -> Nothing
  Rejected AccountedUpstream -> Nothing
  Rejected (RecordLocally reason) -> Just reason

-- | Record a rejected attempt against its items, then apply the discard
-- callback once per category.
--
-- Client-report items carry no data category, so 'Discard.recordEnvelope'
-- skips them and a failed report never reports itself.
recordOutcome :: Maybe ClientReports -> Maybe Discard.Callback -> Patrol.Envelope -> Outcome -> IO ()
recordOutcome reports callback envelope outcome =
  for_ (discardReason outcome) \reason ->
    Discard.recordEnvelope reports callback reason envelope

-- | An attempt the sender took responsibility for, with no rate limits
-- announced.
accepted :: Outcome
accepted = acceptedWith []

-- | An accepted attempt that also announced rate limits, such as a successful
-- response that still carries @X-Sentry-Rate-Limits@.
acceptedWith :: [RateLimit] -> Outcome
acceptedWith rateLimits = Outcome Accepted rateLimits

-- | A rejected attempt this SDK should account for locally, with no rate
-- limits announced.
rejected :: DiscardReason -> Outcome
rejected reason = Outcome (Rejected (RecordLocally reason)) []

-- | A rejected attempt Sentry has already accounted for, such as a 429
-- response, carrying the rate limits it announced.
throttled :: [RateLimit] -> Outcome
throttled rateLimits = Outcome (Rejected AccountedUpstream) rateLimits

-- | A restriction covering every category, expiring at the given instant.
allCategoriesUntil :: UTCTime -> RateLimit
allCategoriesUntil expiresAt = RateLimit AllCategories expiresAt

-- | A restriction covering a single category, expiring at the given instant.
categoryUntil :: DataCategory -> UTCTime -> RateLimit
categoryUntil category expiresAt = RateLimit (Category category) expiresAt
