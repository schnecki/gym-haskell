{-# LANGUAGE OverloadedStrings #-}
module ML.Gym.Range
  ( GymRange(..)
  , getGymRange
  ) where

import           ML.Gym.DType
import           ML.Gym.Util

import           Control.Applicative
import qualified Control.Exception        as E
import           Control.Monad            (void, (>=>))
import qualified CPython                  as Py
import qualified CPython.Constants        as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types            as Py
import qualified CPython.Types.Exception  as Py
import qualified CPython.Types.Module     as Py
import qualified Data.ByteString          as BS
import           Data.Complex             (Complex)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import           System.Environment       (getArgs, getProgName)
import           System.Exit
import           System.IO
import           System.Random            (randomRIO)


data GymRange = GymDoubleRange Double Double
              | GymDoubleArrayRange [Double] [Double]
              | GymIntegerRange Integer Integer
              | GymIntegerArrayRange [Integer] [Integer]
              | GymBoolRange
              | GymBoolArrayRange Int -- ^ Holds length of array
              deriving (Show, Eq)


getGymRange :: Py.Object space => GymDType -> space -> IO (Maybe GymRange)
getGymRange  GymBool    = getGymRange' pyToBool (const . const $ GymBoolRange) (\xs _ -> GymBoolArrayRange (length xs))
getGymRange  GymInteger = getGymRange' pyToInteger GymIntegerRange GymIntegerArrayRange
getGymRange  GymFloat   = getGymRange' pyToFloat GymDoubleRange GymDoubleArrayRange
getGymRange  GymObject  = getGymRange' pyToFloat GymDoubleRange GymDoubleArrayRange
getGymRange tp          = error $ "cannot make range of type " ++ show tp

numPyArray :: Py.SomeObject -> IO Py.SomeObject
numPyArray obj = python (Py.callMethodArgs obj "tolist" [])

getGymRange' :: Py.Object space => (Py.SomeObject -> IO (Maybe a)) -> (a -> a -> GymRange) -> ([a] -> [a] -> GymRange)-> space -> IO (Maybe GymRange)
getGymRange' convert gymRange gymRangeArray space = do
  simple <- getLoHg convert gymRange space
  list <- getLoHg (numPyArray >=> pyToListOf convert) gymRangeArray space
  return $ simple <|> list
  where
    getLoHg convert gymRange space = do
      lo <- lowerBound space >>= convert
      hg <- upperBound space >>= convert
      return $ pure gymRange <*> lo <*> hg


lowerBound :: Py.Object space => space -> IO Py.SomeObject
lowerBound space = python (Py.toUnicode "low" >>= Py.getAttribute space)

upperBound :: Py.Object space => space -> IO Py.SomeObject
upperBound space = python (Py.toUnicode "high" >>= Py.getAttribute space)
