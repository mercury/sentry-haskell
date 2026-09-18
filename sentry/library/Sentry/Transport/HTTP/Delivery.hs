-- | HTTP results and their interpretation as SDK delivery policy.
module Sentry.Transport.HTTP.Delivery (Outcome (..), interpret, interpretNow, retryAfter, sentryHeader) where

import Control.Monad (guard)
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Char (isAscii)
import Data.Foldable (asum)
import Data.Kind (Type)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Network.HTTP.Types qualified as HttpTypes
import Patrol.Type.DataCategory (DataCategory)
import Patrol.Type.DataCategory qualified as DataCategory
import Sentry.ClientReport qualified as ClientReport
import Sentry.Transport.Delivery qualified as Delivery

-- | The response metadata or normalized transport failure, available before policy.
type Outcome :: Type
data Outcome = NetworkFailure Text | Responded HttpTypes.Status HttpTypes.ResponseHeaders
  deriving stock (Show)

-- | Interpret an HTTP result as SDK delivery policy.
--
-- Rate limits are read in the precedence Sentry specifies, independently of
-- the status, because their rate limiting header may appear on any response:
--
--   1. @X-Sentry-Rate-Limits@, if it yields at least one limit.
--   2. @Retry-After@, applied to all categories.
--   3. On a @429@ with neither, a sixty-second limit on all categories.
--
-- See <https://develop.sentry.dev/sdk/expected-features/rate-limiting/>.
interpret :: UTCTime -> Outcome -> Delivery.Outcome
interpret now = \case
  NetworkFailure _ -> Delivery.rejected ClientReport.NetworkError
  Responded status headers -> Delivery.Outcome (disposition status) (limits status headers)
  where
    disposition status
      | status == HttpTypes.tooManyRequests429 = Delivery.Rejected Delivery.AccountedUpstream
      | let code = HttpTypes.statusCode status in 200 <= code && code < 300 = Delivery.Accepted
      | otherwise = Delivery.Rejected (Delivery.RecordLocally ClientReport.SendError)

    limits status headers
      | announced@(_ : _) <- maybe [] (sentryHeader now) (lookup "X-Sentry-Rate-Limits" headers) = announced
      | Just value <- lookup "Retry-After" headers = retryAfter now value
      | status == HttpTypes.tooManyRequests429 = [Delivery.allCategoriesUntil (addUTCTime defaultDelay now)]
      | otherwise = []

-- | Interpret a response, computing rate-limit deadlines from a timestamp
-- taken after the response arrives.
interpretNow :: Outcome -> IO Delivery.Outcome
interpretNow outcome = flip interpret outcome <$> getCurrentTime

-- | Interpret a Retry-After value, retaining numeric, HTTP-date, and fallback behavior.
retryAfter :: UTCTime -> ByteString -> [Delivery.RateLimit]
retryAfter now value = [Delivery.allCategoriesUntil (parseRetryAfter value now)]

-- | Interpret supported categories, ignoring unknown tokens and malformed groups.
sentryHeader :: UTCTime -> ByteString -> [Delivery.RateLimit]
sentryHeader now value = case Atto.parseOnly rateLimitsParser value of
  -- Defensive: rateLimitsParser cannot fail. takeWhile (/= ',') succeeds on
  -- empty input and the inner parseOnly call it wraps returns Maybe, so a
  -- malformed group is skipped rather than propagated as a parse failure.
  Left _ -> []
  Right groups ->
    [ maybe Delivery.allCategoriesUntil Delivery.categoryUntil category (addUTCTime duration now)
    | (duration, categories) <- groups,
      category <- categories
    ]

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
    Right duration -> duration `addUTCTime` now
    Left _ -> case parseHttpDate (ByteString.Char8.unpack value) of
      Just expiresAt -> expiresAt
      Nothing -> defaultDelay `addUTCTime` now
  where
    secondsParser :: Parser NominalDiffTime
    secondsParser = Atto.skipSpace *> finiteDuration <* Atto.skipSpace <* Atto.endOfInput

-- | Reject non-finite seconds before converting to 'NominalDiffTime'.
finiteDuration :: Parser NominalDiffTime
finiteDuration = do
  seconds <- Atto.double
  guard . not $ isNaN seconds || isInfinite seconds
  pure $ realToFrac seconds

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
      duration <- finiteDuration
      _ <- Atto.char ':'
      cats <- categoryToken `Atto.sepBy` Atto.char ';'
      -- Require the scope separator to be present, then ignore the scope,
      -- reason, and namespace fields entirely.
      _ <- Atto.char ':'
      pure (duration, classifyCategories cats)

    categoryToken :: Parser ByteString
    categoryToken = Atto.takeWhile \c -> c /= ':' && c /= ';'

-- | Resolve raw category tokens to 'DataCategory' values.
--
-- An empty token denotes the global catch-all (@Nothing@); unrecognized
-- and non-ASCII tokens are dropped. ASCII case is ignored.
classifyCategories :: [ByteString] -> [Maybe DataCategory]
classifyCategories = mapMaybe classify
  where
    classify :: ByteString -> Maybe (Maybe DataCategory)
    classify token
      | ByteString.Char8.null token = Just Nothing
      | otherwise = do
          guard $ ByteString.Char8.all isAscii token
          Just <$> DataCategory.fromText (Text.toLower $ Text.Encoding.decodeLatin1 token)
