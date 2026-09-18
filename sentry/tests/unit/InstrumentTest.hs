module InstrumentTest where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Fixtures (testEnvelope)
import Network.HTTP.Types qualified as Http
import Sentry.Transport.Delivery qualified as Delivery
import Sentry.Transport.Encoding qualified as Encoding
import Sentry.Transport.HTTP.Delivery qualified as HTTPDelivery
import Sentry.Transport.Instrument qualified as Instrument
import Test.Hspec
import UnliftIO.Exception (throwIO, tryAny)

-- | The observer sees the attempt; the caller sees only the sender's result.
spec_observation :: Spec
spec_observation = describe "observing a delivery attempt" do
  it "returns the sender's outcome unchanged" do
    observed <- newIORef Nothing
    result <-
      Instrument.observing (writeIORef observed . Just) Encoding.Gzip (\_ -> pure ("sentinel" :: String)) testEnvelope
    result `shouldBe` "sentinel"
    Just attempt <- readIORef observed
    attempt.outcome `shouldBe` "sentinel"

  -- The size an observer reports must be the length of the bytes the sender
  -- was handed, which is what 'PublicComponentsTest' shows reaching the wire.
  for_ [Encoding.None, Encoding.Gzip] \compression ->
    it ("reports the prepared size and compression: " <> show compression) do
      observed <- newIORef Nothing
      _ <- Instrument.observing (writeIORef observed . Just) compression (\_ -> pure ()) testEnvelope
      Just attempt <- readIORef observed
      -- Gzip must actually have been applied, or the two encodings would be
      -- indistinguishable and this assertion would prove nothing.
      case compression of
        Encoding.Gzip -> attempt.size `shouldNotBe` LBS.length (Encoding.bytes (Encoding.encode Encoding.None testEnvelope))
        Encoding.None -> attempt.size `shouldBe` LBS.length (Encoding.bytes (Encoding.encode Encoding.None testEnvelope))

  it "times the send" do
    observed <- newIORef Nothing
    _ <-
      Instrument.observing (writeIORef observed . Just) Encoding.Gzip (\_ -> threadDelay 20_000) testEnvelope
    Just attempt <- readIORef observed
    attempt.elapsed `shouldSatisfy` (>= 0.02)

-- | Instrumentation must not be able to affect delivery.
spec_isolation :: Spec
spec_isolation = describe "observer isolation" do
  it "discards observer exceptions" do
    result <-
      Instrument.observing (\_ -> throwIO (userError "metrics backend down")) Encoding.Gzip (\_ -> pure ()) testEnvelope
    result `shouldBe` ()

  it "does not observe an attempt whose sender threw" do
    called <- newIORef False
    outcome <-
      tryAny $
        Instrument.observing
          (\_ -> writeIORef called True)
          Encoding.Gzip
          (\_ -> throwIO (userError "boom") :: IO ())
          testEnvelope
    outcome `shouldSatisfy` either (const True) (const False)
    -- Nothing was reported, because there was no outcome to report.
    readIORef called `shouldReturn` False

-- | One combinator serves every backend, because the outcome type is free.
--
-- This is the regression guard for the module's protocol independence: an
-- earlier design fixed the outcome to the HTTP one, which would have forced a
-- non-HTTP backend to fabricate a status.
spec_protocolIndependence :: Spec
spec_protocolIndependence = describe "outcome type is a parameter" do
  it "instantiates at the HTTP outcome and composes into delivery policy" do
    let httpOutcome = HTTPDelivery.Responded Http.status200 [("Retry-After", "60")]
    void $
      Instrument.observing (\_ -> pure ()) Encoding.Gzip (\_ -> pure httpOutcome) testEnvelope
        >>= HTTPDelivery.interpretNow

  it "instantiates at the generic outcome with no HTTP values in play" do
    let accepted = Delivery.Outcome Delivery.Accepted []
    outcome <-
      Instrument.observing (\_ -> pure ()) Encoding.None (\_ -> pure accepted) testEnvelope
    outcome `shouldBe` accepted
