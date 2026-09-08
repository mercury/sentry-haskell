{-# LANGUAGE OverloadedLabels #-}

module OpticsTest where

import Data.Maybe (isJust)
import Patrol.Type.BreadcrumbType qualified as Patrol.BreadcrumbType
import Patrol.Type.Level qualified as Patrol.Level
import Sentry.Client.Options (defaultClientOptions)
import Sentry.Core.Optics qualified as Sentry
import Sentry.Core.Optics.Prelude
import Sentry.Scope qualified as Scope
import Sentry.Test qualified as Test
import Test.Hspec

spec_editScope :: Spec
spec_editScope = describe "Sentry.Core.Optics (qualified) + Sentry.Core.Optics.Prelude (unqualified)" do
  it "edits a live scope: qualified verbs/values, unqualified operators/labels/values" do
    scope <- Scope.create Sentry.Current
    Sentry.editScope scope do
      #level ?= #error
      #transaction ?= "checkout"
      #tags % at "env" ?= "prod"
      #user ?= (Sentry.emptyUser & #email .~ "alice@example.com")
    d <- Scope.readScopeRef scope
    (d ^. #level) `shouldBe` Just Patrol.Level.Error
    (d ^. #transaction) `shouldBe` Just "checkout"
    (d ^. #tags % at "env") `shouldBe` Just "prod"
    fmap (^. #email) (d ^. #user) `shouldBe` Just "alice@example.com"

spec_valueLabels :: Spec
spec_valueLabels = describe "#-labels as enum values are type-directed by the optic" do
  it "#error resolves to Level under #level and to BreadcrumbType under #type_" do
    scope <- Scope.create Sentry.Current
    Sentry.editScope scope (#level ?= #error)
    d <- Scope.readScopeRef scope
    (d ^. #level) `shouldBe` Just Patrol.Level.Error
    ((Sentry.emptyBreadcrumb & #type_ ?~ #error) ^. #type_)
      `shouldBe` Just Patrol.BreadcrumbType.Error

spec_editValue :: Spec
spec_editValue = describe "(&~) runs an editScope-style block over a plain value" do
  it "builds a record from an empty value with do-notation" do
    let crumb =
          Sentry.emptyBreadcrumb &~ do
            #type_ ?= #navigation
            #category .= "ui"
            #message .= "clicked pay"
    (crumb ^. #type_) `shouldBe` Just Patrol.BreadcrumbType.Navigation
    (crumb ^. #category) `shouldBe` "ui"
    (crumb ^. #message) `shouldBe` "clicked pay"

spec_scopedClient :: Spec
spec_scopedClient = describe "scoped lifecycle re-export" do
  it "keeps core scoped clients transport-agnostic" $ Test.withGlobalScope do
    Sentry.withScopedClient defaultClientOptions do
      client <- Scope.resolveClient
      isJust client.transport `shouldBe` False
    isJust <$> Scope.lookupClient `shouldReturn` False

spec_captureFacade :: Spec
spec_captureFacade = describe "capture facade" do
  it "exports capture outcomes and transport acceptance" do
    Sentry.withClient Sentry.NON_RECORDING_CLIENT $
      Sentry.captureEvent Sentry.emptyEvent `shouldReturn` Nothing
