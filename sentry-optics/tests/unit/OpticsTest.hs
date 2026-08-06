{-# LANGUAGE OverloadedLabels #-}

module OpticsTest where

import Patrol.Type.BreadcrumbType qualified as Patrol.BreadcrumbType
import Patrol.Type.Level qualified as Patrol.Level
import Sentry.Optics qualified as Sentry
import Sentry.Optics.Prelude
import Sentry.Scope qualified as Scope
import Test.Hspec

spec_editScope :: Spec
spec_editScope = describe "Sentry.Optics (qualified) + Sentry.Optics.Prelude (unqualified)" do
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
