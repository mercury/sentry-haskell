-- | Transport-agnostic result of an envelope delivery attempt.
--
-- Both the HTTP\/1.1 and HTTP\/2 transport backends produce an 'Outcome' from
-- their send operations, allowing the rate limiter and client-report logic to
-- be shared across backends without coupling them to a specific HTTP library.
--
-- Designed to be imported qualified:
--
-- > import Sentry.Transport.Delivery qualified as Delivery
-- >
-- > foo :: … -> Delivery.Outcome
-- > reason = Delivery.discardReason outcome
module Sentry.Transport.Delivery
  ( -- * Delivery outcome
    Outcome (..),

    -- * Interpreting outcomes
    discardReason,
  )
where

import Data.Kind (Type)
import Data.Text (Text)
import Network.HTTP.Types (ResponseHeaders, Status, statusCode, tooManyRequests429)
import Sentry.ClientReport (DiscardReason)
import Sentry.ClientReport qualified as ClientReport

-- | Transport-agnostic outcome of an envelope send attempt.
type Outcome :: Type
data Outcome
  = -- | Network or transport-level failure.
    NetworkFailure Text
  | -- | A response was received.
    Responded Status ResponseHeaders
  deriving stock (Show)

-- | Map an 'Outcome' to the 'DiscardReason' that should be recorded for the
-- envelope's items, if any.
--
-- * A successful send (2xx) records nothing.
-- * HTTP 429 records nothing — the rate limiter accounts for it via
--   'Sentry.Transport.Executor.RateLimiter.updateFromResponse', and the items
--   are retried, not dropped.
-- * Any other non-2xx status is a 'ClientReport.SendError'.
-- * A network failure is a 'ClientReport.NetworkError'.
discardReason :: Outcome -> Maybe DiscardReason
discardReason = \case
  Responded status _
    | status == tooManyRequests429 -> Nothing
    | let sc = statusCode status in 200 <= sc && sc < 300 -> Nothing
    | otherwise -> Just ClientReport.SendError
  NetworkFailure _ -> Just ClientReport.NetworkError
