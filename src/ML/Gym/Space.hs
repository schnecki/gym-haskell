

module ML.Gym.Space where

import           ML.Gym.Range
import           ML.Gym.Shape

class ActionCount a where
  actionCount :: a -> Integer


data GymSpace = Discrete Integer
              | Box GymRange GymShape
              deriving (Show, Eq)


instance ActionCount GymSpace where
  actionCount (Discrete nr) = nr


