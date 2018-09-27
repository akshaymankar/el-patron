module Model.PoolSpec where

import Test.Hspec

import Model.Pool

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec :: Spec
spec = do
  describe "Pool" $ do
    context "makePools" $ do
      it "should make pools from list of pool names" $ do
        makePools ["pool-1", "pool-2", "pool3"] `shouldBe` [Pool "pool-1" False, Pool "pool-2" False, Pool "pool3" False]

      it "should filter out lifecycle pools" $ do
        makePools ["pool-1", "pool-2", "pool3", "pool-1-lifecycle"] `shouldBe` [Pool "pool-1" True, Pool "pool-2" False, Pool "pool3" False]
