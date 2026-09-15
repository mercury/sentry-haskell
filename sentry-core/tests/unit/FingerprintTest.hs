module FingerprintTest where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.Default (def)
import Data.Text (Text)
import Sentry.Capture qualified as Capture
import Sentry.Event qualified as E
import Sentry.Event.Captured (CapturedEvent (..))
import Sentry.Scope qualified as S
import Sentry.Scope.IO qualified as IO
import Sentry.Scope.Update qualified as SU
import Sentry.Test qualified as Test
import Sentry.Update qualified as U
import Test.Hspec

spec_fingerprints :: Spec
spec_fingerprints = describe "fingerprints" do
  it "composes insertions and whole-list transformations with duplicates" do
    let e = E.apply E.empty (E.appendFingerprintComponent "b" <> E.prependFingerprintComponent "a" <> E.appendFingerprintComponent "b" <> E.modifyFingerprint (reverse . (<> ["c"])))
        s = U.run [S.appendFingerprintComponent "b", S.prependFingerprintComponent "a", S.appendFingerprintComponent "b", S.modifyFingerprint (reverse . (<> ["c"]))] (def :: S.ScopeData)
    e.fingerprint `shouldBe` ["c", "b", "b", "a"]
    s.fingerprint `shouldBe` Just e.fingerprint
  it "ensures defaults idempotently and preserves spelling and position" do
    E.defaultFingerprintComponent `shouldBe` S.defaultFingerprintComponent
    forM_ ["{{ default }}", "{{default}}"] \token -> do
      let values = ["a", token, "b", token]
      (E.apply E.empty [E.setFingerprint values, E.ensureDefaultFingerprint, E.ensureDefaultFingerprint]).fingerprint `shouldBe` values
      (U.run [S.setFingerprint values, S.ensureDefaultFingerprint, S.ensureDefaultFingerprint] (def :: S.ScopeData)).fingerprint `shouldBe` Just values
    (E.apply E.empty [E.setFingerprint ["a"], E.ensureDefaultFingerprint, E.ensureDefaultFingerprint]).fingerprint `shouldBe` [E.defaultFingerprintComponent, "a"]
    (U.run S.ensureDefaultFingerprint (def :: S.ScopeData)).fingerprint `shouldBe` Just [E.defaultFingerprintComponent]
  it "removes both spellings without normalizing other text" do
    let values :: [Text]
        values = ["{{default}}", "a", "{{ default }}", "{{ default  }}", "{{default}}"]
    (E.apply E.empty [E.setFingerprint values, E.removeDefaultFingerprint]).fingerprint `shouldBe` ["a", "{{ default  }}"]
    (U.run [S.setFingerprint values, S.removeDefaultFingerprint] (def :: S.ScopeData)).fingerprint `shouldBe` Just ["a", "{{ default  }}"]
  it "distinguishes absence from present-empty and skips absent functions" do
    (U.run S.removeDefaultFingerprint (def :: S.ScopeData)).fingerprint `shouldBe` Nothing
    (U.run (S.modifyExistingFingerprint (error "unused")) (def :: S.ScopeData)).fingerprint `shouldBe` Nothing
    (U.run (S.modifyFingerprint id) (def :: S.ScopeData)).fingerprint `shouldBe` Just []
    (U.run [S.ensureDefaultFingerprint, S.removeDefaultFingerprint] (def :: S.ScopeData)).fingerprint `shouldBe` Just []
    (U.run S.clearFingerprint (def :: S.ScopeData)).fingerprint `shouldBe` Just []
    (U.run [S.clearFingerprint, S.unsetFingerprint] (def :: S.ScopeData)).fingerprint `shouldBe` Nothing
  it "forces assignments and results to WHNF without forcing list elements" do
    forM_ [E.appendFingerprintComponent, E.prependFingerprintComponent] \builder ->
      evaluate (E.apply E.empty (builder (error "component"))) `shouldThrow` anyErrorCall
    forM_ [S.appendFingerprintComponent, S.prependFingerprintComponent] \builder ->
      evaluate (U.run (builder (error "component")) (def :: S.ScopeData)) `shouldThrow` anyErrorCall
    forM_ [E.setFingerprint (error "list"), E.modifyFingerprint (const (error "list"))] \upd ->
      evaluate (E.apply E.empty upd) `shouldThrow` anyErrorCall
    forM_ [S.setFingerprint (error "list"), S.modifyFingerprint (const (error "list")), S.setFingerprint [] <> S.modifyExistingFingerprint (const (error "list"))] \upd ->
      evaluate (U.run upd (def :: S.ScopeData)) `shouldThrow` anyErrorCall
    evaluate (length (E.apply E.empty (E.setFingerprint [error "leaf"])).fingerprint) `shouldReturn` 1
    evaluate (fmap length (U.run (S.modifyFingerprint (const [error "leaf"])) (def :: S.ScopeData)).fingerprint) `shouldReturn` Just 1
  it "keeps cloned edits local and does not resurrect suspended current values" do
    IO.withIsolationScope \isolation -> do
      SU.apply isolation (S.setFingerprint ["isolation"])
      IO.withScope \outer -> do
        SU.apply outer (S.setFingerprint ["outer"])
        IO.withScope \inner -> do
          SU.apply inner (S.appendFingerprintComponent "inner")
          (.fingerprint) <$> S.readScopeRef outer `shouldReturn` Just ["outer"]
          (.fingerprint) <$> S.readScopeRef inner `shouldReturn` Just ["outer", "inner"]
          SU.apply inner S.unsetFingerprint
          (.fingerprint) <$> S.readMergedScope `shouldReturn` Just ["isolation"]
        (.fingerprint) <$> S.readScopeRef isolation `shouldReturn` Just ["isolation"]
  it "does not materialize isolation data into an empty current layer" do
    IO.withIsolationScope \isolation -> do
      SU.apply isolation (S.setFingerprint ["isolation"])
      IO.withScope \current -> do
        SU.apply current S.unsetFingerprint
        SU.apply current (S.appendFingerprintComponent "current")
        (.fingerprint) <$> S.readScopeRef current `shouldReturn` Just ["current"]
        (.fingerprint) <$> S.readScopeRef isolation `shouldReturn` Just ["isolation"]
  it "keeps most-specific present scope fingerprints, including empty" do
    let lower = U.run (S.setFingerprint ["lower"]) (def :: S.ScopeData)
        cleared = U.run S.clearFingerprint (def :: S.ScopeData)
    (lower <> cleared).fingerprint `shouldBe` Just []
    (lower <> (def :: S.ScopeData)).fingerprint `shouldBe` Just ["lower"]
  forM_ [[], ["{{ default }}"], ["{{default}}"], ["custom"], ["{{default}}", "custom"], ["{{default}}", "{{default}}"]] \eventFp ->
    forM_ [Nothing, Just [], Just ["scope"]] \scopeFp ->
      it ("captures precedence for " <> show (eventFp, scopeFp)) do
        (_, transport) <- Test.withClient \_ -> IO.withScope \scope -> do
          SU.apply scope (maybe S.unsetFingerprint S.setFingerprint scopeFp)
          Capture.captureEvent (E.apply E.empty (E.setFingerprint eventFp))
        events <- Test.fetchAndClearEvents transport
        let fallback = eventFp `elem` [[], ["{{ default }}"], ["{{default}}"]]
            expected = if fallback then maybe eventFp id scopeFp else eventFp
        map (.fingerprint) events `shouldBe` [expected]
  it "delivers processor edits after scope merging" do
    (_, transport) <- Test.withClient \_ -> IO.withScope \scope -> do
      SU.apply scope [S.setFingerprint ["scope"], S.setEventProcessor (\ce -> Just (E.apply ce.event (E.modifyFingerprint (\fp -> if fp == ["scope"] then [] else ["wrong merge"]))))]
      Capture.captureEvent E.empty
    events <- Test.fetchAndClearEvents transport
    map (.fingerprint) events `shouldBe` [[]]
