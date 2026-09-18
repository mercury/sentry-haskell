{-# OPTIONS_GHC -Wno-x-sentry-experimental #-}

-- | Byte-fidelity contract tests for the transport pieces a caller can
-- assemble themselves.
--
-- The guarantee an instrumented sender depends on is that the bytes it
-- measured are the bytes that shipped, so this drives both backends against a
-- real sink and compares what arrived with what was prepared.
module PublicComponentsTest where

import Control.Exception (bracket)
import Data.Default (def)
import Data.Foldable (for_)
import Network.Connection (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types qualified as Status
import Sentry.TestKit.Gen qualified as Gen
import Sentry.TestKit.Sink qualified as Sink
import Sentry.Transport.Encoding qualified as Encoding
import Sentry.Transport.HTTP.Delivery qualified as Delivery
import Sentry.Transport.HTTP.Request qualified as Request
import Sentry.Transport.HTTP.Sync qualified as HTTP1
import Sentry.Transport.HTTP2.Connection qualified as HTTP2
import System.Timeout (timeout)
import Test.Hspec

spec_publicBodies :: Spec
spec_publicBodies = describe "public encoded bodies and native requests" do
  for_ [Encoding.None, Encoding.Gzip] \compression ->
    it ("sends the same prepared bytes with matching headers on both backends: " <> show compression) $
      bounded $
        Sink.withSink \sink -> do
          let dsn = Sink.dsnFor sink "1"
              envelope = Gen.sampleEnvelope dsn
              encoded = Encoding.encode compression envelope
              body = Encoding.fromBytes compression (Encoding.bytes encoded)
              template = Request.prepare dsn
              -- Deliberately the opposite of the body's encoding: the request
              -- builders must follow the body, not this default.
              endpoint = HTTP2.mkEndpoint Encoding.None dsn
              request = Request.attach template body
          withHTTP1 \manager -> HTTP1.sendRequest manager mempty request >>= assertOK
          bracket (HTTP2.newManager endpoint False 5_000_000 def (pure HTTP2.DontReconnect)) HTTP2.closeManager \manager ->
            HTTP2.sendRequest manager (HTTP2.buildRequest endpoint body) >>= assertOK
          received <- Sink.received sink
          length received `shouldBe` 2
          for_ received \r -> do
            r.body `shouldBe` Encoding.bytes body
            Sink.decodeBody r `shouldBe` Encoding.bytes (Encoding.encode Encoding.None envelope)
            lookup "content-encoding" r.headers `shouldBe` case compression of
              Encoding.None -> Nothing
              Encoding.Gzip -> Just "gzip"

-- | @http-client@ closes managers itself once they fall out of use, so this
-- deliberately does not call the deprecated 'HTTP.closeManager'.
withHTTP1 :: (HTTP.Manager -> IO a) -> IO a
withHTTP1 action =
  HTTP.newManager (mkManagerSettings (TLSSettingsSimple True False False def) Nothing) >>= action

assertOK :: Delivery.Outcome -> IO ()
assertOK = \case
  Delivery.Responded status _ -> status `shouldBe` Status.status200
  Delivery.NetworkFailure err -> expectationFailure (show err)

bounded :: IO () -> IO ()
bounded action = timeout 15_000_000 action `shouldReturn` Just ()
