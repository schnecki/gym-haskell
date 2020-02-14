{-# LANGUAGE OverloadedStrings #-}
module Init.GymSpec
    ( spec
    ) where

import           Test.Hspec

import           ML.Gym

spec :: Spec
spec = do
  describe "initialize Gym environment" $ do
    it "CartPole-v1" $ do
      (ob, gym) <- initGym "CartPole-v1"
      spaceSize (actionSpace gym) `shouldBe` 2
      spaceSize (observationSpace gym) `shouldBe` 4


