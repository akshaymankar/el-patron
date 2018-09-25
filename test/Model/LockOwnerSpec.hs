{-# LANGUAGE OverloadedStrings #-}

module Model.LockOwnerSpec where

import Test.Hspec

import Data.Attoparsec.Text as A
import Data.Either
import Model.LockOwner

{-# ANN module ("HLint: ignore Redundant do"::String) #-}

spec :: Spec
spec = do
  describe "LockOwner" $ do
    context "pipelineParser" $ do
      it "should parse a commit made by pool resource" $ do
        parseOnly pipelineParser "pipeline/job build 42 claiming: the-lock" `shouldBe`
          Right (Pipeline "pipeline" "job" 42)
    context "gafferUserParser" $ do
      it "should parse a commit made by gaffer on behalf of user" $ do
        parseOnly gafferUserParser "someuser: Move something to somewhere" `shouldBe`
          Right (GafferUser "someuser")
