-- | End-to-end coverage for the piggyback failure path: a client report that
-- rides along on a normal event send must be charged under 'Internal' when that
-- send fails, so the loss surfaces on a later successful delivery (matching the
-- official SDKs, e.g. sentry-python's @"internal"@ data category).
--
-- The synchronous HTTP transport piggybacks with @force = True@ on every send,
-- so it exercises the attach-and-fail path deterministically (the async
-- executor only piggybacks once 'piggybackInterval' has elapsed, which is
-- awkward to drive in a test).
module ClientReportFailurePiggybackTest where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Default (def)
import Data.Foldable (toList)
import Network.HTTP.Types qualified as Http
import Patrol.Type.DataCategory qualified as DataCategory
import Sentry.ClientReport qualified as ClientReport
import Sentry.TestKit.Gen qualified as Gen
import Sentry.TestKit.Sink qualified as Sink
import Sentry.Transport qualified as Transport
import Sentry.Transport.HTTP.Sync qualified as SyncHttp
import Test.Hspec

spec_piggybackFailure :: Spec
spec_piggybackFailure = describe "client report piggybacked onto a failed send" do
  it "charges the undelivered report under Internal so it is re-reported" $
    Sink.withSink \sink -> do
      manager <- Sink.tlsManager
      let dsn = Sink.dsnFor sink "1"
      -- A pending discard from an earlier locally-dropped event; the next send
      -- will piggyback it.
      cr <- ClientReport.new
      ClientReport.record cr ClientReport.SampleRate DataCategory.Error 1
      transport <- SyncHttp.build def (Just cr) manager dsn

      -- Fail the first request (the event + piggybacked SampleRate report);
      -- accept everything after.
      Sink.setResponder sink \i _ ->
        pure
          if i == 0
            then Sink.SinkResponse{Sink.status = Http.internalServerError500, Sink.responseHeaders = []}
            else Sink.ok

      -- First send fails: its piggybacked report is lost, but the failure is
      -- charged — the event under Error, the attached report under Internal.
      Transport.send transport (Gen.sampleEnvelope dsn) >>= (`shouldBe` Transport.SendProcessed)
      -- Second send succeeds: it piggybacks the freshly-charged report (which now
      -- carries the Internal entry) and delivers it to the sink.
      Transport.send transport (Gen.sampleEnvelope dsn) >>= (`shouldBe` Transport.SendProcessed)

      reqs <- Sink.received sink
      length reqs `shouldBe` 2
      -- The first request's report carried no Internal entry; only the
      -- re-reported one does, and it reached the sink on the successful send.
      any (any reportsInternal . Sink.envelopeValues) reqs `shouldBe` True

-- | Does this envelope value carry a @discarded_events@ list mentioning the
-- @internal@ category? (The @client_report@ item payload is the only one that
-- does.)
reportsInternal :: Aeson.Value -> Bool
reportsInternal = \case
  Aeson.Object obj
    | Just (Aeson.Array events) <- KeyMap.lookup "discarded_events" obj ->
        or
          [ category == Aeson.String "internal"
          | Aeson.Object event <- toList events,
            Just category <- [KeyMap.lookup "category" event]
          ]
  _ -> False
