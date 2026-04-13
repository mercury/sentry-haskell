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

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Kind (Type)
import Data.List qualified as List
import Data.Maybe (isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Network.HTTP.Types qualified as HttpTypes
import OpenTelemetry.Instrumentation.HttpClient qualified as HttpClient
import Patrol qualified
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Item qualified as Patrol.Item
import Patrol.Type.Items qualified as Patrol.Items
import Text.Read (readMaybe)

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
    logItem :: Maybe UTCTime
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
      logItem = Nothing
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
updateFrom429 rateLimiter now = rateLimiter{global = Just $ 60 `addUTCTime` now}

-- | Update rate limits from a @Retry-After@ header value:
--
-- * Supports both numeric seconds and HTTP-date format
-- * Defaults to 60 seconds if parsing fails
-- * Updates the global rate limit
updateFromRetryAfter :: RateLimiter -> UTCTime -> ByteString -> RateLimiter
updateFromRetryAfter rateLimiter now value =
  let expiresAt = parseRetryAfter (ByteString.Char8.unpack value) now
   in rateLimiter{global = Just $ maxTime rateLimiter.global expiresAt}

-- | Parse @Retry-After@ header value.
--
-- Tries parsing as seconds first, then as HTTP-date; falls back to 60 seconds
-- on failure to parse.
parseRetryAfter :: String -> UTCTime -> UTCTime
parseRetryAfter str now = case readMaybe str of
  Just seconds -> (fromInteger seconds) `addUTCTime` now
  Nothing -> case parseTimeM True defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" str of
    Just retryAfterTime -> retryAfterTime
    Nothing -> 60 `addUTCTime` now

-- | Update rate limits from @X-Sentry-Rate-Limits@ header value.
--
-- Format:
--   @\<rate-limit\> = (\<group\>,)+@
--   @\<group\> = \<time\>:(\<category\>;):\<scope\>:(:\<reason\>)@
updateFromSentryHeader :: RateLimiter -> UTCTime -> ByteString -> RateLimiter
updateFromSentryHeader rateLimiter now value =
  let groups = ByteString.Char8.split ',' value
   in List.foldl' processGroup rateLimiter groups
  where
    processGroup :: RateLimiter -> ByteString -> RateLimiter
    processGroup rl group =
      List.foldl'
        (\accum (category, dt) -> updateCategory (dt `addUTCTime` now) category accum)
        rl
        (processRateLimitGroup group)

-- | Process a single rate limit group from the header.
--
-- Format: @\<time\>:(\<category\>;):\<scope\>:(:\<reason\>)@
--
-- Multiple rate limits are comma-separated. Empty categories apply globally.
processRateLimitGroup :: ByteString -> [(RateLimitingCategory, NominalDiffTime)]
processRateLimitGroup group = case ByteString.Char8.split ':' group of
  (durationStr : categoriesStr : _scopeStr : _) ->
    let duration = parseRateLimitDuration (ByteString.Char8.unpack durationStr)
        categories = parseCategories categoriesStr
     in map (,duration) categories
  _ -> []
  where
    -- Parse duration from rate limit string (seconds as integer).
    parseRateLimitDuration :: String -> NominalDiffTime
    parseRateLimitDuration str = case readMaybe str of
      Just seconds -> fromInteger seconds
      Nothing -> 60

-- | Parse semicolon-separated category names; if none are present, that
-- implies the 'Any' category.
parseCategories :: ByteString -> [RateLimitingCategory]
parseCategories bs =
  let categoryStrs = ByteString.Char8.split ';' bs
      categoryTexts = map (Text.strip . Text.Encoding.decodeUtf8) categoryStrs
   in if null categoryStrs
        then [Any]
        else mapMaybe parseCategoryName categoryTexts
  where
    -- Parse a single category name.
    parseCategoryName :: Text -> Maybe RateLimitingCategory
    parseCategoryName name = case Text.toLower name of
      "" -> Just Any
      "error" -> Just Error
      "session" -> Just Session
      "transaction" -> Just Transaction
      "attachment" -> Just Attachment
      "log_item" -> Just LogItem
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
{-# INLINABLE isDisabledFor #-}

-- | Check if a category is currently allowed (not rate limited).
isEnabled :: UTCTime -> RateLimitingCategory -> RateLimiter -> Bool
isEnabled now category rateLimiter =
  isNothing $ isDisabledFor now category rateLimiter
{-# INLINABLE isEnabled #-}

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
{-# INLINABLE filterEnvelope #-}

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
{-# INLINABLE filterItems #-}

-- | Extract the rate limiting category from an envelope item's type header.
categoryFromItem :: Patrol.Item -> RateLimitingCategory
categoryFromItem = \case
  Patrol.Item.Event _ -> Error
  Patrol.Item.Raw -> Any

-- | Helper to get the maximum of an optional and provided 'UTCTime'.
maxTime :: Maybe UTCTime -> UTCTime -> UTCTime
maxTime Nothing t = t
maxTime (Just t1) t2 = max t1 t2
