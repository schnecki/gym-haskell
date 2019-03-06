{-# LANGUAGE OverloadedStrings #-}
module ML.Gym.Data
  ( GymData(..)
  , getGymData
  , gymObservationToDoubleList
  ) where

import           ML.Gym.DType
import           ML.Gym.Range
import           ML.Gym.Space
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


data GymData =  GymDataDouble Double
              | GymDataInteger Integer
              | GymDataBool Bool
              | GymDataDoubleList [Double]
              | GymDataIntegerList [Integer]
              | GymDataBoolList [Bool]
              deriving (Show, Eq)

gymObservationToDoubleList :: GymData -> [Double]
gymObservationToDoubleList (GymDataDouble x)       = [x]
gymObservationToDoubleList (GymDataInteger x)      = [fromIntegral x]
gymObservationToDoubleList (GymDataBool x)         = [if x then 1 else 0]
gymObservationToDoubleList (GymDataDoubleList xs)  = xs
gymObservationToDoubleList (GymDataIntegerList xs) = map fromIntegral xs
gymObservationToDoubleList (GymDataBoolList xs)    = map (\x -> if x then 1 else 0) xs


getGymData :: GymSpace -> Py.SomeObject -> IO (Maybe GymData)
getGymData (Discrete nr) obj = fmap GymDataInteger <$> pyToInteger obj
getGymData (Box range _) obj = getGymDataRange range obj

getGymDataRange :: GymRange -> Py.SomeObject -> IO (Maybe GymData)
getGymDataRange (GymDoubleRange lo hi) obj         = fmap GymDataDouble <$> pyToDouble obj
getGymDataRange (GymIntegerRange lo hi) obj        = fmap GymDataInteger <$> pyToInteger obj
getGymDataRange GymBoolRange obj                   = fmap GymDataBool <$> pyToBool obj
getGymDataRange (GymDoubleArrayRange los his) obj  = fmap GymDataDoubleList <$> (numPyArray obj >>= pyToListOf pyToDouble)
getGymDataRange (GymIntegerArrayRange los his) obj = fmap GymDataIntegerList <$> (numPyArray obj >>= pyToListOf pyToInteger)
getGymDataRange (GymBoolArrayRange len) obj        = fmap GymDataBoolList <$> (numPyArray obj >>= pyToListOf pyToBool)

numPyArray :: Py.SomeObject -> IO Py.SomeObject
numPyArray obj = python $ Py.callMethodArgs obj "tolist" []

