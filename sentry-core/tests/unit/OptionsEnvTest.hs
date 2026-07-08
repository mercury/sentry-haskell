module OptionsEnvTest where

import Data.Default (def)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Client.Options.Env qualified as Env
import Sentry.Test qualified as Test
import Test.Hspec

resolvePure :: [(String, String)] -> ClientOptions -> (ClientOptions, [Env.Warning])
resolvePure kvs opts = runIdentity (Env.resolveWith look opts)
  where
    env :: Map String String
    env = Map.fromList kvs
    look k = Identity (Map.lookup k env)

-- | Convenience: resolve from 'def' and keep only the options.
resolvedFrom :: [(String, String)] -> ClientOptions
resolvedFrom kvs = fst (resolvePure kvs def)

spec_optionsEnv :: Spec
spec_optionsEnv = describe "environment-variable resolution" do
  describe "populates unset fields from the environment" do
    it "reads SENTRY_DSN" do
      let opts = resolvedFrom [("SENTRY_DSN", "https://public@sentry.invalid/1")]
      opts.dsn `shouldSatisfy` isJust

    it "reads SENTRY_RELEASE" do
      (resolvedFrom [("SENTRY_RELEASE", "1.2.3")]).release `shouldBe` Just "1.2.3"

    it "reads SENTRY_ENVIRONMENT" do
      (resolvedFrom [("SENTRY_ENVIRONMENT", "staging")]).environment `shouldBe` Just "staging"

    it "reads SENTRY_DEBUG" do
      (resolvedFrom [("SENTRY_DEBUG", "1")]).debug `shouldBe` Just True

    it "reads SENTRY_SAMPLE_RATE" do
      (resolvedFrom [("SENTRY_SAMPLE_RATE", "0.25")]).sampleRate `shouldBe` Just 0.25

    it "reads SENTRY_TRACES_SAMPLE_RATE" do
      (resolvedFrom [("SENTRY_TRACES_SAMPLE_RATE", "0.5")]).tracesSampleRate `shouldBe` Just 0.5

    it "reads SENTRY_PROFILES_SAMPLE_RATE" do
      (resolvedFrom [("SENTRY_PROFILES_SAMPLE_RATE", "0.125")]).profilesSampleRate `shouldBe` Just 0.125

  describe "code configuration takes strict precedence" do
    it "keeps an explicit dsn over SENTRY_DSN" do
      let opts = def{dsn = Just Test.TEST_DSN}
      (fst (resolvePure [("SENTRY_DSN", "https://other@sentry.invalid/2")] opts)).dsn
        `shouldBe` Just Test.TEST_DSN

    it "keeps an explicit environment over SENTRY_ENVIRONMENT" do
      let opts = def{environment = Just "explicit"}
      (fst (resolvePure [("SENTRY_ENVIRONMENT", "staging")] opts)).environment
        `shouldBe` Just "explicit"

    it "keeps an explicit debug=Just False over SENTRY_DEBUG=1" do
      let opts = def{debug = Just False}
      (fst (resolvePure [("SENTRY_DEBUG", "1")] opts)).debug `shouldBe` Just False

    it "keeps an explicit sampleRate=Just 0.0 over SENTRY_SAMPLE_RATE=1" do
      let opts = def{sampleRate = Just 0.0}
      (fst (resolvePure [("SENTRY_SAMPLE_RATE", "1")] opts)).sampleRate `shouldBe` Just 0.0

  describe "terminal defaults when neither code nor environment set a value" do
    it "defaults environment to production" do
      (resolvedFrom []).environment `shouldBe` Just "production"

    it "defaults debug to False" do
      (resolvedFrom []).debug `shouldBe` Just False

    it "defaults sampleRate to 1.0" do
      (resolvedFrom []).sampleRate `shouldBe` Just 1.0

    it "leaves tracesSampleRate/profilesSampleRate unset (no default)" do
      let opts = resolvedFrom []
      (opts.tracesSampleRate, opts.profilesSampleRate) `shouldBe` (Nothing, Nothing)

  describe "DSN parsing" do
    it "parses a valid DSN with no warnings" do
      let (opts, warnings) = resolvePure [("SENTRY_DSN", "https://public@sentry.invalid/1")] def
      opts.dsn `shouldSatisfy` isJust
      warnings `shouldBe` []

    it "ignores a malformed DSN and warns (without throwing)" do
      let (opts, warnings) = resolvePure [("SENTRY_DSN", "not a url")] def
      opts.dsn `shouldBe` Nothing
      warnings `shouldBe` [Env.MalformedDsn "not a url"]

  describe "sample-rate parsing" do
    it "clamps values above 1 down to 1" do
      (resolvedFrom [("SENTRY_SAMPLE_RATE", "2.0")]).sampleRate `shouldBe` Just 1.0

    it "clamps negative values up to 0" do
      (resolvedFrom [("SENTRY_SAMPLE_RATE", "-1")]).sampleRate `shouldBe` Just 0.0

    it "ignores unparseable values, warns, and falls back to the default" do
      let (opts, warnings) = resolvePure [("SENTRY_SAMPLE_RATE", "abc")] def
      opts.sampleRate `shouldBe` Just 1.0
      warnings `shouldBe` [Env.MalformedRate "SENTRY_SAMPLE_RATE" "abc"]

    it "warns for a malformed traces rate and leaves the field unset" do
      let (opts, warnings) = resolvePure [("SENTRY_TRACES_SAMPLE_RATE", "nope")] def
      opts.tracesSampleRate `shouldBe` Nothing
      warnings `shouldBe` [Env.MalformedRate "SENTRY_TRACES_SAMPLE_RATE" "nope"]

  describe "debug parsing" do
    it "recognises truthy and falsy spellings case-insensitively" do
      map Env.parseBool ["1", "true", "YES", "On", "0", "false", "No", "OFF"]
        `shouldBe` [Just True, Just True, Just True, Just True, Just False, Just False, Just False, Just False]

    it "treats an empty SENTRY_DEBUG as unset (no warning)" do
      let (opts, warnings) = resolvePure [("SENTRY_DEBUG", "")] def
      opts.debug `shouldBe` Just False
      warnings `shouldBe` []

    it "warns for an unrecognised SENTRY_DEBUG value and falls back to the default" do
      let (opts, warnings) = resolvePure [("SENTRY_DEBUG", "maybe")] def
      opts.debug `shouldBe` Just False
      warnings `shouldBe` [Env.UnrecognizedBool "maybe"]

  describe "parseRate helper" do
    it "parses and clamps" do
      map Env.parseRate ["0.0", "0.5", "1.0", "2.0", "-3", "bogus"]
        `shouldBe` [Just 0.0, Just 0.5, Just 1.0, Just 1.0, Just 0.0, Nothing]
