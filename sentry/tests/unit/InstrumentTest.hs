module InstrumentTest where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (AsyncCancelled, async, cancel, waitCatch)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, fromException)
import Control.Monad (void)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Fixtures (testEnvelope)
import Network.HTTP.Types qualified as Http
import Sentry.Transport.Delivery qualified as Delivery
import Sentry.Transport.Encoding qualified as Encoding
import Sentry.Transport.HTTP.Delivery qualified as HTTPDelivery
import Sentry.Transport.Instrument qualified as Instrument
import System.Timeout (timeout)
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
    either (const Nothing) Just attempt.outcome `shouldBe` Just "sentinel"

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

  it "observes a sender exception and preserves it when the observer fails" do
    observed <- newIORef Nothing
    let failure = userError "sender failed"
    outcome <-
      tryAny $
        Instrument.observing
          (\attempt -> writeIORef observed (Just attempt) >> throwIO (userError "observer failed"))
          Encoding.Gzip
          (\_ -> threadDelay 20_000 >> throwIO failure :: IO ())
          testEnvelope
    either fromException (const Nothing) outcome `shouldBe` Just failure
    Just attempt <- readIORef observed
    either fromException (const Nothing) attempt.outcome `shouldBe` Just failure
    attempt.elapsed `shouldSatisfy` (>= 0.02)
    attempt.size `shouldBe` (Encoding.encode Encoding.Gzip testEnvelope).size

  it "observes cancellation and propagates it" do
    started <- newEmptyMVar
    blocked <- newEmptyMVar
    observed <- newIORef Nothing
    worker <-
      async $
        Instrument.observing
          (writeIORef observed . Just)
          Encoding.Gzip
          (\_ -> putMVar started () >> takeMVar blocked :: IO ())
          testEnvelope
    timeout 1_000_000 (takeMVar started) `shouldReturn` Just ()
    timeout 1_000_000 (cancel worker) `shouldReturn` Just ()
    result <- waitCatch worker
    result `shouldSatisfy` isFailure
    Just attempt <- readIORef observed
    attempt.outcome `shouldSatisfy` isFailure

  for_ [False, True] \senderFails ->
    it ("propagates observer cancellation after sender failure: " <> show senderFails) do
      started <- newEmptyMVar
      blocked <- newEmptyMVar
      worker <-
        async $
          Instrument.observing
            (\_ -> putMVar started () >> takeMVar blocked)
            Encoding.None
            (\_ -> if senderFails then throwIO (userError "sender failed") else pure ())
            testEnvelope
      timeout 1_000_000 (takeMVar started) `shouldReturn` Just ()
      timeout 1_000_000 (cancel worker) `shouldReturn` Just ()
      result <- waitCatch worker
      either (fmap (const True) . fromException @AsyncCancelled) (const Nothing) result `shouldBe` Just True

  it "does not observe encoding failures" do
    called <- newIORef False
    _ <-
      tryAny $
        Instrument.observing
          (\_ -> writeIORef called True)
          Encoding.Gzip
          (\_ -> pure ())
          (error "encoding failed")
    readIORef called `shouldReturn` False

isFailure :: Either SomeException a -> Bool
isFailure = either (const True) (const False)

-- | One combinator serves every backend, because the outcome type is free.
--
-- This is the regression guard for the module's protocol independence: an
-- earlier design fixed the outcome to the HTTP one, which would have forced a
-- non-HTTP backend to fabricate a status.
spec_protocolIndependence :: Spec
spec_protocolIndependence = describe "outcome type is a parameter" do
  it "instantiates at the HTTP outcome and composes into delivery policy" do
    let httpOutcome = HTTPDelivery.Responded (UTCTime (toEnum 0) 0) Http.status200 [("Retry-After", "60")]
    void $
      Instrument.observing (\_ -> pure ()) Encoding.Gzip (\_ -> pure httpOutcome) testEnvelope
        >>= (pure . HTTPDelivery.interpret)

  it "instantiates at the generic outcome with no HTTP values in play" do
    let accepted = Delivery.Outcome Delivery.Accepted []
    outcome <-
      Instrument.observing (\_ -> pure ()) Encoding.None (\_ -> pure accepted) testEnvelope
    outcome `shouldBe` accepted

-- | Observer work cannot move the timestamp carried by the HTTP response.
spec_responseTiming :: Spec
spec_responseTiming = describe "response timing through instrumentation" do
  it "retains the header timestamp when the observer advances time" do
    let receivedAt = UTCTime (toEnum 0) 0
    clock <- newIORef receivedAt
    let sender _ = do
          now <- readIORef clock
          pure $ HTTPDelivery.Responded now Http.status200 [("Retry-After", "60")]
        observe _ = writeIORef clock (addUTCTime 120 receivedAt)
    response <- Instrument.observing observe Encoding.None sender testEnvelope
    readIORef clock `shouldReturn` addUTCTime 120 receivedAt
    (HTTPDelivery.interpret response).rateLimits
      `shouldBe` [Delivery.allCategoriesUntil (addUTCTime 60 receivedAt)]
