module OptionsEnvTest where

import Data.Default (def)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as Text
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Client.Options.Dsn qualified as Dsn
import Sentry.Client.Options.Env qualified as Env
import Sentry.Test qualified as Test
import Test.Hspec

resolvePure :: [(String, String)] -> ClientOptions -> (ClientOptions, [Env.Warning])
resolvePure kvs = Env.resolveSnapshot (Env.EnvSnapshot kvs)

-- | Convenience: resolve from 'def' and keep only the options.
resolvedFrom :: [(String, String)] -> ClientOptions
resolvedFrom kvs = fst (resolvePure kvs def)

spec_optionsEnv :: Spec
spec_optionsEnv = describe "environment-variable resolution" do
  describe "populates unset fields from the environment" do
    it "reads SENTRY_DSN" do
      let opts = resolvedFrom [("SENTRY_DSN", "https://public@sentry.invalid/1")]
      opts.dsn
        `shouldSatisfy` ( \case
                            Dsn.Explicit _ -> True
                            _ -> False
                        )

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
      let opts = def{dsn = Dsn.Explicit Test.TEST_DSN}
      (fst (resolvePure [("SENTRY_DSN", "https://other@sentry.invalid/2")] opts)).dsn
        `shouldBe` Dsn.Explicit Test.TEST_DSN

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
      opts.dsn
        `shouldSatisfy` ( \case
                            Dsn.Explicit _ -> True
                            _ -> False
                        )
      warnings `shouldBe` []

    it "ignores a malformed DSN and warns (without throwing)" do
      let (opts, warnings) = resolvePure [("SENTRY_DSN", "not a url")] def
      opts.dsn `shouldBe` Dsn.Disabled
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

spec_finalization :: Spec
spec_finalization = describe "configuration finalization" do
  it "reads each environment variable once into a reusable snapshot" do
    calls <- newIORef ([] :: [String])
    snapshot <- Env.snapshotWith \key -> do
      modifyIORef' calls (<> [key])
      pure (Just "value")
    seen <- readIORef calls
    let expectedKeys :: [String]
        expectedKeys =
          [ "SENTRY_DSN",
            "SENTRY_RELEASE",
            "SENTRY_ENVIRONMENT",
            "SENTRY_DEBUG",
            "SENTRY_SAMPLE_RATE",
            "SENTRY_TRACES_SAMPLE_RATE",
            "SENTRY_PROFILES_SAMPLE_RATE"
          ]
    seen `shouldBe` expectedKeys
    snapshot `shouldBe` Env.EnvSnapshot [(key, "value") | key <- expectedKeys]
    let (opts, warnings) = Env.resolveSnapshot snapshot def
    opts.release `shouldBe` Just "value"
    warnings
      `shouldBe` [ Env.MalformedDsn "value",
                   Env.UnrecognizedBool "value",
                   Env.MalformedRate "SENTRY_SAMPLE_RATE" "value",
                   Env.MalformedRate "SENTRY_TRACES_SAMPLE_RATE" "value",
                   Env.MalformedRate "SENTRY_PROFILES_SAMPLE_RATE" "value"
                 ]

  it "does not diagnose shadowed malformed environment values" do
    let opts = def{dsn = Dsn.Disabled, debug = Just False, sampleRate = Just 0, tracesSampleRate = Just 0, profilesSampleRate = Just 0}
        vars :: [(String, String)]
        vars = [(key, "bad") | key <- ["SENTRY_DSN", "SENTRY_DEBUG", "SENTRY_SAMPLE_RATE", "SENTRY_TRACES_SAMPLE_RATE", "SENTRY_PROFILES_SAMPLE_RATE"]]
        (resolved, warnings) = resolvePure vars opts
    resolved.dsn `shouldBe` Dsn.Disabled
    warnings `shouldBe` []

  it "treats absent and empty inherited DSNs as disabled without diagnostics" do
    map (\vars -> let (o, w) = resolvePure vars def in (o.dsn, w)) [[], [("SENTRY_DSN", "")]]
      `shouldBe` [(Dsn.Disabled, []), (Dsn.Disabled, [])]

  it "uses terminal defaults for cleared setup fields, retaining only DSN inheritance" do
    let snapshot :: [(String, String)]
        snapshot = [("SENTRY_DSN", "https://public@sentry.invalid/1"), ("SENTRY_DEBUG", "true"), ("SENTRY_ENVIRONMENT", "staging"), ("SENTRY_SAMPLE_RATE", "0.5"), ("SENTRY_TRACES_SAMPLE_RATE", "0.5")]
        (opts, warnings) = Env.finalize (Env.EnvSnapshot snapshot) def
    opts.dsn
      `shouldSatisfy` ( \case
                          Dsn.Explicit _ -> True
                          _ -> False
                      )
    (opts.debug, opts.environment, opts.sampleRate, opts.tracesSampleRate) `shouldBe` (Just False, Just "production", Just 1, Nothing)
    warnings `shouldBe` []

  it "clamps finite code rates quietly, including boundaries" do
    map (\rate -> let (o, w) = resolvePure [] def{sampleRate = Just rate} in (o.sampleRate, w)) [-2, 0, 0.5, 1, 2]
      `shouldBe` [(Just 0, []), (Just 0, []), (Just 0.5, []), (Just 1, []), (Just 1, [])]

  it "defaults non-finite code rates without falling back to environment rates" do
    mapM_
      ( \rate -> do
          let (opts, warnings) =
                resolvePure
                  [("SENTRY_SAMPLE_RATE", "0"), ("SENTRY_TRACES_SAMPLE_RATE", "0.5"), ("SENTRY_PROFILES_SAMPLE_RATE", "0.5")]
                  def{sampleRate = Just rate, tracesSampleRate = Just rate, profilesSampleRate = Just rate}
          (opts.sampleRate, opts.tracesSampleRate, opts.profilesSampleRate) `shouldBe` (Just 1, Nothing, Nothing)
          warnings
            `shouldBe` [ Env.InvalidRateOption field (Text.pack (show rate))
                       | field <- ["sampleRate", "tracesSampleRate", "profilesSampleRate"]
                       ]
      )
      [0 / 0, 1 / 0, -1 / 0]

  it "rejects non-finite and malformed environment rates with diagnostics" do
    mapM_
      ( \raw -> do
          let (opts, warnings) = resolvePure [("SENTRY_SAMPLE_RATE", raw), ("SENTRY_TRACES_SAMPLE_RATE", raw), ("SENTRY_PROFILES_SAMPLE_RATE", raw)] def
          (opts.sampleRate, opts.tracesSampleRate, opts.profilesSampleRate) `shouldBe` (Just 1, Nothing, Nothing)
          warnings
            `shouldBe` [ Env.MalformedRate variable (Text.pack raw)
                       | variable <- ["SENTRY_SAMPLE_RATE", "SENTRY_TRACES_SAMPLE_RATE", "SENTRY_PROFILES_SAMPLE_RATE"]
                       ]
      )
      ["NaN", "Infinity", "-Infinity", "1e1000", "bad"]

spec_warningRendering :: Spec
spec_warningRendering = describe "configuration diagnostic rendering" do
  it "distinguishes environment variables from option fields" do
    map
      Env.renderWarning
      [ Env.MalformedDsn "bad",
        Env.UnrecognizedBool "maybe",
        Env.MalformedRate "SENTRY_SAMPLE_RATE" "bad",
        Env.InvalidRateOption "sampleRate" "NaN"
      ]
      `shouldBe` [ "ignoring malformed SENTRY_DSN: bad",
                   "ignoring unrecognised SENTRY_DEBUG value: maybe",
                   "ignoring malformed SENTRY_SAMPLE_RATE: bad",
                   "ignoring invalid option sampleRate value: NaN"
                 ]

  it "diagnoses non-finite setup options during pure finalization" do
    let initialOptions = def{sampleRate = Just (1 / 0)}
        (opts, warnings) = Env.finalize (Env.EnvSnapshot []) initialOptions
    opts.sampleRate `shouldBe` Just 1
    warnings `shouldBe` [Env.InvalidRateOption "sampleRate" "Infinity"]
