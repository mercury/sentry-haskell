{-# LANGUAGE NumericUnderscores #-}

-- | Rate limiting for Sentry transports based on server responses.
--
-- This implementation mirrors the Rust SDK's rate limiter:
-- <https://github.com/getsentry/sentry-rust/blob/master/sentry/src/transports/ratelimit.rs>
--
-- Sentry uses rate limiting headers to control request rates:
--
-- * @X-Sentry-Rate-Limits@: Category-specific rate limits
-- * @Retry-After@: Global rate limit
-- * HTTP 429: Default (60 second) rate limit
--
-- Example usage:
--
-- > let initialLimiter = RateLimiter.new
--
-- > -- After receiving response with rate limit headers
-- > let limiter = RateLimiter.updateFromSentryHeader initialLimiter currentTime headers
--
-- > -- Check if category is rate limited
-- > when (RateLimiter.isEnabled currentTime limiter RateLimitingCategory.Error) $
-- >   sendEvent event
--
-- > -- Or filter envelope items
-- > whenJust (RateLimiter.filterEnvelope currentTime limiter envelope) \filtered ->
-- >   sendEnvelope filtered
module Sentry.Transport.Executor.RateLimiter
  ( -- * Rate Limiter
    RateLimiter (..),
    new,

    -- * Rate Limiting Categories
    RateLimitingCategory (..),

    -- * Updating Rate Limits
    updateFromResponse,
    updateFrom429,
    updateFromRetryAfter,
    updateFromSentryHeader,

    -- * Querying Rate Limits
    isDisabledUntil,
    isDisabledFor,
    isEnabled,

    -- * Filtering payloads
    filterEnvelope,
  )
where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Char (isSpace, toLower)
import Data.Foldable (asum)
import Data.Kind (Type)
import Data.List qualified as List
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Network.HTTP.Types qualified as HttpTypes
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Item qualified as Patrol.Item
import Patrol.Type.Items qualified as Patrol.Items

-- | Categories for rate limiting in Sentry.
--
-- Each category can be rate limited independently based on server responses.
type RateLimitingCategory :: Type
data RateLimitingCategory
  = -- | Applies globally to all event types.
    Any
  | -- | Error events (exceptions, crashes, etc.).
    Error
  | -- | Session health data.
    Session
  | -- | Performance monitoring transactions.
    Transaction
  | -- | File attachments.
    Attachment
  | -- | Log messages.
    LogItem
  | -- | Metric buckets emitted on the trace metrics protocol.
    TraceMetric
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded)

-- | Rate limiter tracking active rate limits per category.
--
-- This is a pure value that gets threaded through the transport worker loop.
--
-- Use 'new' to create an initial instance and 'updateFromRetryAfter' as well
-- as 'updateFromSentryHeader' to incorporate  rate limit information from
-- server responses.
type RateLimiter :: Type
data RateLimiter = RateLimiter
  { global :: Maybe UTCTime,
    error_ :: Maybe UTCTime,
    session :: Maybe UTCTime,
    transaction :: Maybe UTCTime,
    attachment :: Maybe UTCTime,
    logItem :: Maybe UTCTime,
    traceMetric :: Maybe UTCTime
  }
  deriving stock (Show)

-- | Create a new rate limiter with no active limits.
new :: RateLimiter
new =
  RateLimiter
    { global = Nothing,
      error_ = Nothing,
      session = Nothing,
      transaction = Nothing,
      attachment = Nothing,
      logItem = Nothing,
      traceMetric = Nothing
    }

-- | Update rate limits from an HTTP response.
--
-- * On success: parses @X-Sentry-Rate-Limits@ and @Retry-After@ headers
-- * On HTTP 429: applies a default 60-second global rate limit
-- * On other errors: returns the rate limiter unchanged
updateFromResponse ::
  RateLimiter ->
  UTCTime ->
  Either HttpClient.HttpExceptionContent (HttpClient.Response ()) ->
  RateLimiter
updateFromResponse rl now = \case
  Right response ->
    let headers = HttpClient.responseHeaders response
        rl' = case lookup "X-Sentry-Rate-Limits" headers of
          Just value -> updateFromSentryHeader rl now value
          Nothing -> rl
        rl'' = case lookup "Retry-After" headers of
          Just value -> updateFromRetryAfter rl' now value
          Nothing -> rl'
     in rl''
  Left (HttpClient.StatusCodeException response _)
    | HttpClient.responseStatus response == HttpTypes.tooManyRequests429 ->
        updateFrom429 rl now
  Left _ -> rl

-- | Update the global rate limit to end 1 minute after the provided time, in
-- response to a HTTP 429 status code.
updateFrom429 :: RateLimiter -> UTCTime -> RateLimiter
updateFrom429 rateLimiter now = rateLimiter{global = Just $ defaultDelay `addUTCTime` now}

-- | Update rate limits from a @Retry-After@ header value:
--
-- * Supports both numeric seconds and HTTP-date format
-- * Defaults to 60 seconds if parsing fails
-- * Updates the global rate limit
updateFromRetryAfter :: RateLimiter -> UTCTime -> ByteString -> RateLimiter
updateFromRetryAfter rateLimiter now value =
  let expiresAt = parseRetryAfter value now
   in rateLimiter{global = Just $ maxTime rateLimiter.global expiresAt}

-- | Parse a @Retry-After@ header value into an absolute expiry time.
--
-- Tried in order:
--
--   1. Numeric seconds (integer or floating point) added to @now@.
--   2. An HTTP-date (RFC 7231), interpreted as an absolute instant.
--   3. Failing both, a default 60-second delay from @now@.
parseRetryAfter :: ByteString -> UTCTime -> UTCTime
parseRetryAfter value now =
  case Atto.parseOnly secondsParser value of
    Right seconds -> realToFrac seconds `addUTCTime` now
    Left _ -> case parseHttpDate (ByteString.Char8.unpack value) of
      Just expiresAt -> expiresAt
      Nothing -> defaultDelay `addUTCTime` now
  where
    secondsParser :: Parser Double
    secondsParser = Atto.skipSpace *> Atto.double <* Atto.skipSpace <* Atto.endOfInput

-- | The fallback rate-limit duration applied when a value cannot be parsed or
-- on a bare HTTP 429 response.
defaultDelay :: NominalDiffTime
defaultDelay = 60

-- | Parse an HTTP-date in any of the three formats permitted by RFC 7231:
-- the preferred IMF-fixdate, the obsolete RFC 850 form, and the asctime form.
parseHttpDate :: String -> Maybe UTCTime
parseHttpDate str = asum [parseTimeM True defaultTimeLocale fmt str | fmt <- formats]
  where
    formats =
      [ -- IMF-fixdate: Sun, 06 Nov 1994 08:49:37 GMT
        "%a, %d %b %Y %H:%M:%S GMT",
        -- RFC 850: Sunday, 06-Nov-94 08:49:37 GMT
        "%A, %d-%b-%y %H:%M:%S GMT",
        -- asctime: Sun Nov  6 08:49:37 1994
        "%a %b %e %H:%M:%S %Y"
      ]

-- | Update rate limits from an @X-Sentry-Rate-Limits@ header value.
--
-- The header is a comma-separated list of groups, each of the form:
--
-- > <retry_after>:<categories>:<scope>[:<reason>[:<namespaces>]]
--
-- where @retry_after@ is a (possibly fractional) number of seconds and
-- @categories@ is a semicolon-separated list (empty meaning all categories).
-- The @scope@, @reason@, and @namespaces@ fields are accepted but ignored.
--
-- Groups that cannot be parsed are skipped; well-formed groups in the same
-- header still apply.
updateFromSentryHeader :: RateLimiter -> UTCTime -> ByteString -> RateLimiter
updateFromSentryHeader rateLimiter now value =
  case Atto.parseOnly rateLimitsParser value of
    Left _ -> rateLimiter
    Right groups -> List.foldl' applyGroup rateLimiter groups
  where
    applyGroup :: RateLimiter -> (NominalDiffTime, [RateLimitingCategory]) -> RateLimiter
    applyGroup rl (duration, categories) =
      List.foldl'
        (\accum category -> updateCategory (duration `addUTCTime` now) category accum)
        rl
        categories

-- | Parse an entire @X-Sentry-Rate-Limits@ header into per-group limits.
--
-- Each group is consumed up to the next comma and parsed independently, so a
-- malformed group yields no limits rather than failing the whole header.
rateLimitsParser :: Parser [(NominalDiffTime, [RateLimitingCategory])]
rateLimitsParser = catMaybes <$> (groupParser `Atto.sepBy` Atto.char ',') <* Atto.endOfInput
  where
    groupParser :: Parser (Maybe (NominalDiffTime, [RateLimitingCategory]))
    groupParser = do
      raw <- Atto.takeWhile (/= ',')
      pure $ either (\_ -> Nothing) Just (Atto.parseOnly groupBody raw)

    groupBody :: Parser (NominalDiffTime, [RateLimitingCategory])
    groupBody = do
      Atto.skipSpace
      seconds <- Atto.double
      _ <- Atto.char ':'
      categories <- categoryToken `Atto.sepBy` Atto.char ';'
      -- Require the scope separator to be present (matching the grammar),
      -- then ignore the scope, reason, and namespace fields entirely.
      _ <- Atto.char ':'
      pure (realToFrac seconds, classifyCategories categories)

    categoryToken :: Parser ByteString
    categoryToken = Atto.takeWhile \c -> c /= ':' && c /= ';'

-- | Resolve raw category tokens to known 'RateLimitingCategory' values.
--
-- An empty token denotes the global 'Any' category; unrecognized tokens are
-- dropped.
classifyCategories :: [ByteString] -> [RateLimitingCategory]
classifyCategories = mapMaybe classify
  where
    classify :: ByteString -> Maybe RateLimitingCategory
    classify token = case ByteString.Char8.map toLower token of
      "" -> Just Any
      "error" -> Just Error
      "session" -> Just Session
      "transaction" -> Just Transaction
      "attachment" -> Just Attachment
      "log_item" -> Just LogItem
      "trace_metric" -> Just TraceMetric
      _ -> Nothing

-- | Update a specific category's expiration time, taking the maximum of the
-- existing value or the given time to update it with.
updateCategory :: UTCTime -> RateLimitingCategory -> RateLimiter -> RateLimiter
updateCategory expiresAt category rateLimiter = case category of
  Any -> rateLimiter{global = Just $ maxTime rateLimiter.global expiresAt}
  Error -> rateLimiter{error_ = Just $ maxTime rateLimiter.error_ expiresAt}
  Session -> rateLimiter{session = Just $ maxTime rateLimiter.session expiresAt}
  Transaction -> rateLimiter{transaction = Just $ maxTime rateLimiter.transaction expiresAt}
  Attachment -> rateLimiter{attachment = Just $ maxTime rateLimiter.attachment expiresAt}
  LogItem -> rateLimiter{logItem = Just $ maxTime rateLimiter.logItem expiresAt}
  TraceMetric -> rateLimiter{traceMetric = Just $ maxTime rateLimiter.traceMetric expiresAt}

-- | Check when a given category is rate limited until.
--
-- Returns @Just utcTime@ if rate limited, where @utcTime@ is the time at which
-- rate limiting expires; returns @Nothing@ if not rate limited.
--
-- Checks global rate limit first, then category-specific limit.
isDisabledUntil :: RateLimiter -> RateLimitingCategory -> Maybe UTCTime
isDisabledUntil rateLimiter category =
  case category of
    Any -> rateLimiter.global
    Error -> select rateLimiter.error_
    Session -> select rateLimiter.session
    Transaction -> select rateLimiter.transaction
    Attachment -> select rateLimiter.attachment
    LogItem -> select rateLimiter.logItem
    TraceMetric -> select rateLimiter.traceMetric
  where
    select :: Maybe UTCTime -> Maybe UTCTime
    select (Just t) = Just $ maxTime rateLimiter.global t
    select mt = case rateLimiter.global of
      Nothing -> mt
      Just t -> Just $ maxTime mt t

-- | Check how many seconds a given 'RateLimitingCategory' is disabled for.
--
-- Returns @Just duration@ if rate limited, where @duration@ is the number of
-- seconds until rate limiting expires with respect to the provided 'UTCTime'.
--
-- Returns @Nothing@ if not rate limited.
isDisabledFor :: UTCTime -> RateLimitingCategory -> RateLimiter -> Maybe NominalDiffTime
isDisabledFor now category rateLimiter = checkExpiry =<< isDisabledUntil rateLimiter category
  where
    checkExpiry expiresAt =
      if now < expiresAt
        then Just $ expiresAt `diffUTCTime` now
        else Nothing
{-# INLINEABLE isDisabledFor #-}

-- | Check if a category is currently allowed (not rate limited).
isEnabled :: UTCTime -> RateLimitingCategory -> RateLimiter -> Bool
isEnabled now category rateLimiter =
  isNothing $ isDisabledFor now category rateLimiter
{-# INLINEABLE isEnabled #-}

-- | Filter envelope items based on current rate limits.
--
-- Removes items whose categories are currently rate limited (leaving any items
-- whose types cannot be determined) and returns 'Nothing' if all items were
-- filtered out.
--
-- Returns @Nothing@ if all items are filtered out.
filterEnvelope :: RateLimiter -> UTCTime -> Patrol.Envelope -> Maybe Patrol.Envelope
filterEnvelope rateLimiter now envelope = do
  filteredItems <- filterItems rateLimiter now envelope.items
  Just envelope{Patrol.Envelope.items = filteredItems}
{-# INLINEABLE filterEnvelope #-}

-- | Filter items based on their rate limit status.
filterItems :: RateLimiter -> UTCTime -> Patrol.Items.Items -> Maybe Patrol.Items.Items
filterItems rateLimiter now = \case
  payload@(Patrol.Items.Raw _) ->
    if isEnabled now Any rateLimiter
      then Just payload
      else Nothing
  Patrol.Items.EnvelopeItems items ->
    let filtered = flip filter items \item -> isEnabled now (categoryFromItem item) rateLimiter
     in case filtered of
          [] -> Nothing
          remaining -> Just $ Patrol.Items.EnvelopeItems remaining
{-# INLINEABLE filterItems #-}

-- | Extract the rate limiting category from an envelope item's type header.
categoryFromItem :: Patrol.Item -> RateLimitingCategory
categoryFromItem = \case
  Patrol.Item.Event _ -> Error
  Patrol.Item.Raw -> Any

-- | Helper to get the maximum of an optional and provided 'UTCTime'.
maxTime :: Maybe UTCTime -> UTCTime -> UTCTime
maxTime Nothing t = t
maxTime (Just t1) t2 = max t1 t2
