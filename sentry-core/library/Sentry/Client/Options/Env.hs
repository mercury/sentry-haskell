-- | Apply environment variable configuration fragments to 'ClientOptions' updates.
--
-- Sentry SDKs read a common set of environment variables so that behaviour can
-- be tuned without code changes (containers, serverless, etc.).
--
-- Per the <https://develop.sentry.dev/sdk/foundations/client/configuration/ configuration spec>,
-- __code configuration takes strict precedence__: if the caller supplied a value
-- the environment variable is ignored entirely.
module Sentry.Client.Options.Env
  ( -- * Resolution
    resolve,
    resolveWith,

    -- * Warnings
    Warning (..),
    renderWarning,

    -- * Parsers
    parseBool,
    parseRate,
  )
where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Sentry.Internal (ClientOptions (..))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | A malformed environment variable that was set but could not be parsed, and
-- was therefore ignored during resolution.
type Warning :: Type
data Warning
  = -- | The raw @SENTRY_DSN@ value that failed to parse.
    MalformedDsn Text
  | -- | The variable name and the raw value that failed to parse as a rate.
    MalformedRate Text Text
  | -- | 'parseBool' could not resolve an environment variable (e.g. @SENTRY_DEBUG@).
    UnrecognizedBool Text
  deriving stock (Eq, Show)

-- | Render a 'Warning' into a human-facing message.
renderWarning :: Warning -> Text
renderWarning = \case
  MalformedDsn raw ->
    "ignoring malformed SENTRY_DSN: " <> raw
  MalformedRate var raw ->
    "ignoring malformed " <> var <> ": " <> raw
  UnrecognizedBool raw ->
    "ignoring unrecognised SENTRY_DEBUG value: " <> raw

-- | Resolve environment-variable defaults into 'ClientOptions', reading from the
-- process environment via 'lookupEnv'.
--
-- Returns the resolved options alongside any 'Warning's for environment
-- variables that were set but malformed (and therefore ignored).
resolve :: ClientOptions -> IO (ClientOptions, [Warning])
resolve = resolveWith lookupEnv

-- | 'resolve' generalised over the environment lookup, so tests can inject a
-- pure lookup instead of touching the real process environment.
resolveWith ::
  (Monad m) =>
  -- | Environment lookup (e.g. 'System.Environment.lookupEnv').
  (String -> m (Maybe String)) ->
  ClientOptions ->
  m (ClientOptions, [Warning])
resolveWith look opts = do
  dsnRaw <- look "SENTRY_DSN"
  releaseRaw <- look "SENTRY_RELEASE"
  envRaw <- look "SENTRY_ENVIRONMENT"
  debugRaw <- look "SENTRY_DEBUG"
  rateRaw <- look "SENTRY_SAMPLE_RATE"
  tracesRaw <- look "SENTRY_TRACES_SAMPLE_RATE"
  profilesRaw <- look "SENTRY_PROFILES_SAMPLE_RATE"
  let dsnEnv = (Patrol.Dsn.fromText . Text.pack) =<< dsnRaw
      debugEnv = parseBool =<< debugRaw
      rateEnv = parseRate =<< rateRaw
      tracesEnv = parseRate =<< tracesRaw
      profilesEnv = parseRate =<< profilesRaw
      warnings =
        catMaybes
          [ warnIf MalformedDsn dsnRaw dsnEnv,
            warnIf UnrecognizedBool debugRaw debugEnv,
            warnIf (MalformedRate "SENTRY_SAMPLE_RATE") rateRaw rateEnv,
            warnIf (MalformedRate "SENTRY_TRACES_SAMPLE_RATE") tracesRaw tracesEnv,
            warnIf (MalformedRate "SENTRY_PROFILES_SAMPLE_RATE") profilesRaw profilesEnv
          ]
  pure
    ( opts
        { dsn = opts.dsn <|> dsnEnv,
          release = opts.release <|> (Text.pack <$> releaseRaw),
          environment = opts.environment <|> (Text.pack <$> envRaw) <|> Just "production",
          debug = opts.debug <|> debugEnv <|> Just False,
          sampleRate = opts.sampleRate <|> rateEnv <|> Just 1.0,
          tracesSampleRate = opts.tracesSampleRate <|> tracesEnv,
          profilesSampleRate = opts.profilesSampleRate <|> profilesEnv
        },
      warnings
    )

-- | Emit a warning when an environment variable was set to a non-empty value
-- that failed to parse.
--
-- A missing or empty variable is treated as \"unset\" and produces no warning.
warnIf :: (Text -> Warning) -> Maybe String -> Maybe a -> Maybe Warning
warnIf mk raw parsed = case raw of
  Just s | not (null s), Nothing <- parsed -> Just (mk (Text.pack s))
  _ -> Nothing

-- | Parse a boolean environment variable, case-insensitively.
--
-- Truthy: @1@, @true@, @yes@, @on@.
-- Falsey: @0@, @false@, @no@, @off@.
--
-- Anything else (including the empty string) yields 'Nothing'.
parseBool :: String -> Maybe Bool
parseBool s
  | v `elem` ["1", "true", "yes", "on"] = Just True
  | v `elem` ["0", "false", "no", "off"] = Just False
  | otherwise = Nothing
  where
    v = map toLower s

-- | Parse a sample-rate environment variable, clamping the result to @[0,1]@.
-- Unparseable input yields 'Nothing'.
parseRate :: String -> Maybe Float
parseRate s = clamp <$> readMaybe s
  where
    clamp :: Float -> Float
    clamp = max 0 . min 1
