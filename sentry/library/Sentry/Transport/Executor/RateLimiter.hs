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
-- Categories map to patrol's 'DataCategory', the same vocabulary used for
-- client-report discard accounting.
--
-- Example usage:
--
-- > let initialLimiter = RateLimiter.new
--
-- > -- After receiving response with rate limit headers
-- > let limiter = RateLimiter.updateFromSentryHeader initialLimiter currentTime headers
--
-- > -- Check if a category is rate limited
-- > when (RateLimiter.isEnabled currentTime (Just DataCategory.Error) limiter) $
-- >   sendEvent event
--
-- > -- Or filter against the current limits, sending only what survives
-- > let filtered = RateLimiter.filterEnvelope limiter currentTime envelope
-- > for_ filtered.kept sendEnvelope
module Sentry.Transport.Executor.RateLimiter
  ( -- * Rate Limiter
    RateLimiter (..),
    new,

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
    FilteredEnvelope (..),
    filterEnvelope,
  )
where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Char (toLower)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Kind (Type)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Network.HTTP.Types qualified as HttpTypes
import Patrol qualified
import Patrol.Type.DataCategory (DataCategory)
import Patrol.Type.DataCategory qualified as DataCategory
import Patrol.Type.Envelope qualified as Patrol.Envelope
import Patrol.Type.Items qualified as Patrol.Items
import Sentry.ClientReport (categoryFromItem)
import Sentry.Transport.Delivery qualified as Delivery

-- | Rate limiter tracking active rate limits per data category.
--
-- 'global' is the catch-all limit applied by an empty @X-Sentry-Rate-Limits@
-- category token, @Retry-After@, or HTTP 429.  'categories' holds
-- per-category expiry times.
--
-- This is a pure value that gets threaded through the transport worker loop.
--
-- Use 'new' to create an initial instance and 'updateFromRetryAfter' as well
-- as 'updateFromSentryHeader' to incorporate rate limit information from
-- server responses.
type RateLimiter :: Type
data RateLimiter = RateLimiter
  { global :: Maybe UTCTime,
    categories :: Map DataCategory UTCTime
  }
  deriving stock (Show)

-- | Create a new rate limiter with no active limits.
new :: RateLimiter
new = RateLimiter{global = Nothing, categories = Map.empty}

-- | Update rate limits from a send 'Outcome'.
--
-- * On 2xx: parses @X-Sentry-Rate-Limits@ and @Retry-After@ headers
-- * On HTTP 429: applies a default 60-second global rate limit
-- * On other non-2xx or network failure: returns the rate limiter unchanged
updateFromResponse ::
  RateLimiter ->
  UTCTime ->
  Delivery.Outcome ->
  RateLimiter
updateFromResponse rl now = \case
  Delivery.NetworkFailure _ -> rl
  Delivery.Responded status headers
    | status == HttpTypes.tooManyRequests429 ->
        updateFrom429 rl now
    | let sc = HttpTypes.statusCode status in 200 <= sc && sc < 300 ->
        let rl' = case lookup "X-Sentry-Rate-Limits" headers of
              Just value -> updateFromSentryHeader rl now value
              Nothing -> rl
            rl'' = case lookup "Retry-After" headers of
              Just value -> updateFromRetryAfter rl' now value
              Nothing -> rl'
         in rl''
    | otherwise -> rl

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
    formats :: [String]
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
    applyGroup :: RateLimiter -> (NominalDiffTime, [Maybe DataCategory]) -> RateLimiter
    applyGroup rl (duration, categories) =
      List.foldl'
        (\accum m -> updateCategory (duration `addUTCTime` now) m accum)
        rl
        categories

-- | Parse an entire @X-Sentry-Rate-Limits@ header into per-group limits.
--
-- Each group is consumed up to the next comma and parsed independently, so a
-- malformed group yields no limits rather than failing the whole header.
rateLimitsParser :: Parser [(NominalDiffTime, [Maybe DataCategory])]
rateLimitsParser = catMaybes <$> (groupParser `Atto.sepBy` Atto.char ',') <* Atto.endOfInput
  where
    groupParser :: Parser (Maybe (NominalDiffTime, [Maybe DataCategory]))
    groupParser = do
      raw <- Atto.takeWhile (/= ',')
      pure $ either (\_ -> Nothing) Just (Atto.parseOnly groupBody raw)

    groupBody :: Parser (NominalDiffTime, [Maybe DataCategory])
    groupBody = do
      Atto.skipSpace
      seconds <- Atto.double
      _ <- Atto.char ':'
      cats <- categoryToken `Atto.sepBy` Atto.char ';'
      -- Require the scope separator to be present (matching the grammar),
      -- then ignore the scope, reason, and namespace fields entirely.
      _ <- Atto.char ':'
      pure (realToFrac seconds, classifyCategories cats)

    categoryToken :: Parser ByteString
    categoryToken = Atto.takeWhile \c -> c /= ':' && c /= ';'

-- | Resolve raw category tokens to 'DataCategory' values.
--
-- An empty token denotes the global catch-all (@Nothing@); unrecognized
-- tokens are dropped.
classifyCategories :: [ByteString] -> [Maybe DataCategory]
classifyCategories = mapMaybe classify
  where
    classify :: ByteString -> Maybe (Maybe DataCategory)
    classify token = case ByteString.Char8.unpack (ByteString.Char8.map toLower token) of
      "" -> Just Nothing -- global catch-all
      s -> fmap Just $ DataCategory.fromText (Text.pack s)

-- | Update a specific category's expiration time, taking the maximum of the
-- existing value or the given time.
--
-- 'Nothing' updates the global limit; 'Just cat' updates the per-category
-- limit for @cat@.
updateCategory :: UTCTime -> Maybe DataCategory -> RateLimiter -> RateLimiter
updateCategory expiresAt Nothing rl =
  rl{global = Just $ maxTime rl.global expiresAt}
updateCategory expiresAt (Just category) rl =
  rl
    { categories =
        Map.insertWith max category expiresAt rl.categories
    }

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

-- | Helper to get the maximum of an optional and provided 'UTCTime'.
maxTime :: Maybe UTCTime -> UTCTime -> UTCTime
maxTime Nothing t = t
maxTime (Just t1) t2 = max t1 t2
