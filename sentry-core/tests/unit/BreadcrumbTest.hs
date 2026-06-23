module BreadcrumbTest where

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Foldable (toList, traverse_)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Patrol qualified
import Patrol.Type.Breadcrumb qualified as Patrol.Breadcrumb
import Patrol.Type.BreadcrumbType qualified as Patrol.BreadcrumbType
import Patrol.Type.Breadcrumbs qualified as Patrol.Breadcrumbs
import Patrol.Type.Event qualified as Patrol.Event
import Sentry.Client (pattern NON_RECORDING_CLIENT)
import Sentry.Client.Options (ClientOptions (..))
import Sentry.Event (CapturedEvent (..))
import Sentry.Scope (ScopeData (..))
import Sentry.Scope qualified as Scope
import Sentry.Scope.IO qualified as Scope.IO
import Sentry.Test qualified as Test
import Test.Hspec
import Witch qualified

spec_breadcrumbs :: Spec
spec_breadcrumbs = do
  describe "Scope.apply breadcrumb merge" do
    it "appends scope breadcrumbs after event breadcrumbs" do
      let eventCrumb = crumb "event"
          scopeCrumb = crumb "scope"
          scope = def{breadcrumbs = Seq.fromList [scopeCrumb]}
          event = Patrol.Event.empty{Patrol.Event.breadcrumbs = Just $ Patrol.Breadcrumbs.Breadcrumbs [eventCrumb]}
          ce = Witch.from @Patrol.Event @CapturedEvent event
      case Scope.apply scope ce of
        Nothing -> expectationFailure "event processor dropped event"
        Just result ->
          result.breadcrumbs
            `shouldBe` Just (Patrol.Breadcrumbs.Breadcrumbs [eventCrumb, scopeCrumb])

    it "omits breadcrumbs entirely when both sides are empty" do
      let ce = Witch.from @Patrol.Event @CapturedEvent Patrol.Event.empty
      case Scope.apply def ce of
        Nothing -> expectationFailure "event processor dropped event"
        Just result -> result.breadcrumbs `shouldBe` Nothing

    it "emits scope breadcrumbs when the event has none" do
      let scopeCrumb = crumb "scope"
          scope = def{breadcrumbs = Seq.fromList [scopeCrumb]}
          ce = Witch.from @Patrol.Event @CapturedEvent Patrol.Event.empty
      case Scope.apply scope ce of
        Nothing -> expectationFailure "event processor dropped event"
        Just result ->
          result.breadcrumbs
            `shouldBe` Just (Patrol.Breadcrumbs.Breadcrumbs [scopeCrumb])

  describe "addBreadcrumb" do
    it "defaults type_ to Default when absent" do
      (scopeData, _) <- Test.withClient \_ ->
        Scope.IO.withIsolationScope \scope -> do
          Scope.addBreadcrumb Patrol.Breadcrumb.empty{Patrol.Breadcrumb.type_ = Nothing}
          liftIO $ Scope.readScopeRef scope
      case toList scopeData.breadcrumbs of
        [c] -> c.type_ `shouldBe` Just Patrol.BreadcrumbType.Default
        cs -> expectationFailure $ "expected 1 crumb, got " <> show (length cs)

    it "preserves an explicit type_" do
      (scopeData, _) <- Test.withClient \_ ->
        Scope.IO.withIsolationScope \scope -> do
          Scope.addBreadcrumb Patrol.Breadcrumb.empty{Patrol.Breadcrumb.type_ = Just Patrol.BreadcrumbType.Http}
          liftIO $ Scope.readScopeRef scope
      case toList scopeData.breadcrumbs of
        [c] -> c.type_ `shouldBe` Just Patrol.BreadcrumbType.Http
        cs -> expectationFailure $ "expected 1 crumb, got " <> show (length cs)

    it "defaults timestamp when absent" do
      (scopeData, _) <- Test.withClient \_ ->
        Scope.IO.withIsolationScope \scope -> do
          Scope.addBreadcrumb Patrol.Breadcrumb.empty{Patrol.Breadcrumb.timestamp = Nothing}
          liftIO $ Scope.readScopeRef scope
      case toList scopeData.breadcrumbs of
        [c] -> c.timestamp `shouldSatisfy` (/= Nothing)
        cs -> expectationFailure $ "expected 1 crumb, got " <> show (length cs)

    it "trims oldest entries to stay within maxBreadcrumbs" do
      (scopeData, _) <- Test.withCustomClient def{maxBreadcrumbs = 3} \_ ->
        Scope.IO.withIsolationScope \scope -> do
          traverse_ Scope.addBreadcrumb [crumb "1", crumb "2", crumb "3", crumb "4", crumb "5"]
          liftIO $ Scope.readScopeRef scope
      map (\c -> c.message) (toList scopeData.breadcrumbs) `shouldBe` ["3", "4", "5"]

    it "beforeBreadcrumb returning Nothing drops the crumb" do
      (scopeData, _) <- Test.withCustomClient def{beforeBreadcrumb = Just (const Nothing)} \_ ->
        Scope.IO.withIsolationScope \scope -> do
          Scope.addBreadcrumb (crumb "dropped")
          liftIO $ Scope.readScopeRef scope
      scopeData.breadcrumbs `shouldBe` mempty

    it "beforeBreadcrumb can modify the crumb" do
      let tweak b = Just b{Patrol.Breadcrumb.message = "modified"}
      (scopeData, _) <- Test.withCustomClient def{beforeBreadcrumb = Just tweak} \_ ->
        Scope.IO.withIsolationScope \scope -> do
          Scope.addBreadcrumb (crumb "original")
          liftIO $ Scope.readScopeRef scope
      case toList scopeData.breadcrumbs of
        [c] -> c.message `shouldBe` "modified"
        cs -> expectationFailure $ "expected 1 crumb, got " <> show (length cs)

    it "still writes to the scope for NON_RECORDING_CLIENT (transport is irrelevant at add-time)" do
      scopeData <- Scope.IO.withClient NON_RECORDING_CLIENT $
        Scope.IO.withIsolationScope \scope -> do
          Scope.addBreadcrumb (crumb "present")
          liftIO $ Scope.readScopeRef scope
      map (.message) (toList scopeData.breadcrumbs) `shouldBe` ["present"]

  describe "addBreadcrumbs" do
    it "adds multiple crumbs in order" do
      (scopeData, _) <- Test.withClient \_ ->
        Scope.IO.withIsolationScope \scope -> do
          Scope.addBreadcrumbs [crumb "a", crumb "b", crumb "c"]
          liftIO $ Scope.readScopeRef scope
      map (.message) (toList scopeData.breadcrumbs) `shouldBe` ["a", "b", "c"]

  describe "clearBreadcrumbs" do
    it "empties the scope breadcrumbs" do
      (scopeData, _) <- Test.withClient \_ ->
        Scope.IO.withIsolationScope \scope -> do
          Scope.addBreadcrumbs [crumb "a", crumb "b"]
          Scope.clearBreadcrumbs scope
          liftIO $ Scope.readScopeRef scope
      scopeData.breadcrumbs `shouldBe` mempty

-- Helpers

-- | Minimal breadcrumb with a distinguishable message.
crumb :: Text -> Patrol.Breadcrumb
crumb msg = Patrol.Breadcrumb.empty{Patrol.Breadcrumb.message = msg}
