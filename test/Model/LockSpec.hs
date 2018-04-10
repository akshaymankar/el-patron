{-# LANGUAGE OverloadedStrings #-}
module Model.LockSpec where

import Test.Hspec
import Model.Lock
import Data.Attoparsec.Text as A
import Data.Either

spec :: Spec
spec = do
  describe "Lock" $ do
    context "commitParser" $ do
      it "should parse a commit made by pool resource" $ do
        parseOnly commitParser "pipeline/job build 42 claiming: the-lock"  `shouldBe` Right (Pipeline "pipeline" "job" 42)
