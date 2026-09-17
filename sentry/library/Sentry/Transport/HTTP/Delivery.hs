-- | HTTP results and their interpretation as SDK delivery policy.
module Sentry.Transport.HTTP.Delivery (Outcome (..), interpret, retryAfter, sentryHeader) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as Atto
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Char (toLower)
import Data.Foldable (asum)
import Data.Kind (Type)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
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
interpret :: UTCTime -> Outcome -> Delivery.Outcome
interpret now = \case
  NetworkFailure _ -> Delivery.rejected ClientReport.NetworkError
  Responded status headers
    | status == HttpTypes.tooManyRequests429 ->
        Delivery.throttled [Delivery.allCategoriesUntil (addUTCTime defaultDelay now)]
    | let code = HttpTypes.statusCode status in 200 <= code && code < 300 ->
        Delivery.acceptedWith $
          maybe [] (sentryHeader now) (lookup "X-Sentry-Rate-Limits" headers)
            <> maybe [] (retryAfter now) (lookup "Retry-After" headers)
    | otherwise -> Delivery.rejected ClientReport.SendError

-- | Interpret a Retry-After value, retaining numeric, date and fallback behavior.
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
