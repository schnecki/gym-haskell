{-# LANGUAGE OverloadedStrings #-}

module ML.Gym.Space where

import           ML.Gym.DType
import           ML.Gym.Range
import           ML.Gym.Shape
import           ML.Gym.Util

import qualified Control.Exception        as E
import           Control.Monad            (void)
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


class Dimension a where
  dimension :: a -> Integer


data GymSpace = Discrete Integer
              | Box GymRange GymShape
              deriving (Show, Eq)


instance Dimension GymSpace where
  dimension (Discrete nr)         = nr
  dimension (Box _ (Gym1D x1))    = x1
  dimension (Box _ (Gym2D x1 x2)) = x1 * x2


getGymRangeFromSpace :: GymSpace -> GymRange
getGymRangeFromSpace (Discrete nr) = GymIntegerRange 0 (nr-1)
getGymRangeFromSpace (Box rng _)   = rng

toGymSpace :: Py.Object self => self -> T.Text -> IO GymSpace
toGymSpace space txt
  | T.isPrefixOf "Discrete" txt = do
      n <- fromMaybe (error "Gym discrete space 'n' not recognized!") <$> python (Py.toUnicode "n" >>= Py.getAttribute space >>= pyToInteger)
      return $ Discrete n
  | T.isPrefixOf "Box" txt = do
      dType <- python $ Py.toUnicode "dtype" >>= Py.getAttribute space >>= getGymDType
      range <- fromMaybe (error "could not get gym range") <$> python (getGymRange dType space)
      shape <- fromMaybe (error "could not get gym shape") <$> python (getGymShape space)
      return $ Box range shape
  | otherwise = do
      putStrLn "Sample space: "
      python (Py.callMethodArgs space "sample" [] >>= flip Py.print stdout)
      error $ "ERROR: Gym space '" ++ T.unpack txt ++ "' not supported yet!"
