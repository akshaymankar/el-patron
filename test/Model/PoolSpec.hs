module Model.PoolSpec where

import Test.Hspec

import Model.Pool

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec :: Spec
spec = do
  describe "Pool" $ do
    context "makePools" $ do
      it "should make pools from list of pool names" $ do
        makePools ["pool-1", "pool-2", "pool3"] `shouldBe` [Pool "pool-1", Pool "pool-2", Pool "pool3"]

      it "should filter out lifecycle pools" $ do
        makePools ["pool-1", "pool-2", "pool3", "pool1-lifecycle"] `shouldBe` [Pool "pool-1", Pool "pool-2", Pool "pool3"]
