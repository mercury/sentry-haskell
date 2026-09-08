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
    snapshotWith,
    EnvSnapshot (..),
    resolveSnapshot,
    finalize,

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
import Data.Maybe (catMaybes, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Patrol.Type.Dsn qualified as Patrol.Dsn
import Sentry.Client.Options.Defaults qualified as Defaults
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.Internal (ClientOptions (..))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Invalid configuration ignored during construction. Rate diagnostics also
-- cover non-finite values supplied by code or integration setup.
type Warning :: Type
data Warning
  = -- | The raw @SENTRY_DSN@ value that failed to parse.
    MalformedDsn Text
  | -- | The variable name and the raw value that failed to parse as a rate.
    MalformedRate Text Text
  | -- | 'parseBool' could not resolve an environment variable (e.g. @SENTRY_DEBUG@).
    UnrecognizedBool Text
  | -- | The option field and its non-finite value.
    InvalidRateOption Text Text
  deriving stock (Eq, Show)

-- | Render a 'Warning' into a human-facing message.
renderWarning :: Warning -> Text
renderWarning = \case
  MalformedDsn raw ->
    "ignoring malformed SENTRY_DSN: " <> raw
  MalformedRate var raw ->
    "ignoring malformed " <> var <> ": " <> raw
  InvalidRateOption field raw ->
    "ignoring invalid option " <> field <> " value: " <> raw
  UnrecognizedBool raw ->
    "ignoring unrecognised SENTRY_DEBUG value: " <> raw

-- | Resolve environment-variable defaults into 'ClientOptions', reading from the
-- process environment via 'lookupEnv'.
--
-- Returns the resolved options alongside any 'Warning's for environment
-- variables that were set but malformed (and therefore ignored).
resolve :: ClientOptions -> IO (ClientOptions, [Warning])
resolve = resolveWith lookupEnv

-- | Environment values captured once for deterministic configuration resolution.
type EnvSnapshot :: Type
newtype EnvSnapshot = EnvSnapshot [(String, String)]
  deriving stock (Eq, Show)

-- | Read each supported variable exactly once.
snapshotWith :: (Monad m) => (String -> m (Maybe String)) -> m EnvSnapshot
snapshotWith look =
  fmap (EnvSnapshot . catMaybes) $
    traverse
      readOne
      ["SENTRY_DSN", "SENTRY_RELEASE", "SENTRY_ENVIRONMENT", "SENTRY_DEBUG", "SENTRY_SAMPLE_RATE"]
  where
    readOne key = fmap ((key,) <$>) (look key)

-- | Finalize setup output with terminal defaults; only DSN inheritance can
-- consult the original snapshot again.
finalize :: EnvSnapshot -> ClientOptions -> (ClientOptions, [Warning])
finalize (EnvSnapshot snapshot) =
  resolveSnapshot (EnvSnapshot (filter ((== "SENTRY_DSN") . fst) snapshot))

-- | 'resolve' generalised over the environment lookup, so tests can inject a
-- pure lookup instead of touching the real process environment.
resolveWith ::
  (Monad m) =>
  -- | Environment lookup (e.g. 'System.Environment.lookupEnv').
  (String -> m (Maybe String)) ->
  ClientOptions ->
  m (ClientOptions, [Warning])
resolveWith look opts = do
  snapshot <- snapshotWith look
  pure (resolveSnapshot snapshot opts)

-- | Resolve options against captured environment values without further effects.
resolveSnapshot :: EnvSnapshot -> ClientOptions -> (ClientOptions, [Warning])
resolveSnapshot (EnvSnapshot snapshot) opts =
  let look key = lookup key snapshot
      dsnRaw = look "SENTRY_DSN"
      releaseRaw = look "SENTRY_RELEASE"
      envRaw = look "SENTRY_ENVIRONMENT"
      debugRaw = look "SENTRY_DEBUG"
      rateRaw = look "SENTRY_SAMPLE_RATE"
      dsnEnv = (Patrol.Dsn.fromText . Text.pack) =<< dsnRaw
      debugEnv = parseBool =<< debugRaw
      rateEnv = parseRate =<< rateRaw
      warnings =
        catMaybes
          [ if opts.dsn == Dsn.Inherit then warnIf MalformedDsn dsnRaw dsnEnv else Nothing,
            if isNothing opts.debug then warnIf UnrecognizedBool debugRaw debugEnv else Nothing,
            rateWarning "SENTRY_SAMPLE_RATE" "sampleRate" opts.sampleRate rateRaw rateEnv
          ]
   in ( opts
          { dsn = case opts.dsn of
              Dsn.Inherit -> maybe Dsn.Disabled Dsn.Explicit dsnEnv
              value -> value,
            release = opts.release <|> (Text.pack <$> releaseRaw),
            environment = opts.environment <|> (Text.pack <$> envRaw) <|> Just Defaults.environment,
            debug = opts.debug <|> debugEnv <|> Just Defaults.debug,
            sampleRate = chooseRate opts.sampleRate rateEnv <|> Just Defaults.sampleRate
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
-- Unparseable or non-finite input yields 'Nothing'.
parseRate :: String -> Maybe Float
parseRate s = readMaybe s >>= normalizeRate

normalizeRate :: Float -> Maybe Float
normalizeRate rate
  | isNaN rate || isInfinite rate = Nothing
  | otherwise = Just (max 0 (min 1 rate))

chooseRate :: Maybe Float -> Maybe Float -> Maybe Float
chooseRate Nothing fallback = fallback
chooseRate (Just rate) _ = normalizeRate rate

rateWarning :: Text -> Text -> Maybe Float -> Maybe String -> Maybe Float -> Maybe Warning
rateWarning name _ Nothing raw parsed = warnIf (MalformedRate name) raw parsed
rateWarning _ field (Just rate) _ _
  | isNothing (normalizeRate rate) = Just (InvalidRateOption field (Text.pack (show rate)))
  | otherwise = Nothing
