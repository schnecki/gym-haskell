{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (forever, unless, void)
import           ML.Gym


main :: IO ()
main = do
  gym <- snd <$> initGym "CartPole-v1"
  forever $ resetGym gym >> runEpisode gym


runEpisode :: Gym -> IO Gym
runEpisode gym = do
  GymResult _ reward episodeDone <- stepGymRandom gym
  if episodeDone
    then return gym
    else runEpisode gym
