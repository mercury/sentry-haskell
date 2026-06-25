{-# LANGUAGE OverloadedLabels #-}

module LabelsTest where

import Optics.Core ((%~), (&), (.~), (^.), (^?))
import Patrol.Optics ()
import Patrol.Type.Context qualified as Context
import Patrol.Type.DeviceContext qualified as DeviceContext
import Patrol.Type.User qualified as User
import Test.Hspec

spec_fieldLabels :: Spec
spec_fieldLabels = describe "patrol-optics field labels" do
  it "sets and gets User.email via #email (a lens)" do
    let u = User.empty & #email .~ "alice@example.com"
    (u ^. #email) `shouldBe` "alice@example.com"

  it "modifies User.username via #username with over" do
    let u = (User.empty & #username .~ "alice") & #username %~ (<> "-2")
    (u ^. #username) `shouldBe` "alice-2"

spec_prismLabels :: Spec
spec_prismLabels = describe "patrol-optics prism labels" do
  it "previews the matching Context constructor via #_Device" do
    let ctx = Context.Device DeviceContext.empty
    (ctx ^? #_Device) `shouldBe` Just DeviceContext.empty

  it "fails to preview a non-matching constructor" do
    let ctx = Context.Device DeviceContext.empty
    (ctx ^? #_Os) `shouldBe` Nothing
