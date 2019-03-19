{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module ML.Gym.Value where

import           ML.Gym.DType
import           ML.Gym.Util

import           Control.Applicative
import qualified Control.Exception        as E
import           Control.Monad            (join, void, (>=>))
import qualified CPython                  as Py
import qualified CPython.Constants        as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types            as Py
import qualified CPython.Types.Exception  as Py
import qualified CPython.Types.Module     as Py
import qualified Data.ByteString          as BS
import           Data.Complex             (Complex)
import           Data.Foldable            (toList)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import           System.Environment       (getArgs, getProgName)
import           System.Exit
import           System.IO
import           System.Random            (randomRIO)

import           Debug.Trace

data GymValue a
  = GymScalar a
  | Gym1D [a]
  | Gym2D [[a]]
  | Gym3D [[[a]]]
  | GymTuple [GymValue a]
  deriving (Show, Eq, Functor)


gymValueTo1D :: GymValue a -> [a]
gymValueTo1D (GymScalar x) = [x]
gymValueTo1D (Gym1D xs)    = xs
gymValueTo1D (Gym2D xs)    = concat xs
gymValueTo1D (Gym3D xs)    = concat $ concat xs
gymValueTo1D (GymTuple xs) = concatMap gymValueTo1D xs


gymValueTo2D :: GymValue a -> [[a]]
gymValueTo2D (GymScalar x) = [[x]]
gymValueTo2D (Gym1D xs)    = [xs]
gymValueTo2D (Gym2D xs)    = xs
gymValueTo2D (Gym3D xs)    = concat xs
gymValueTo2D (GymTuple xs) = map gymValueTo1D xs


gymValueTo3D :: GymValue a -> [[[a]]]
gymValueTo3D (GymScalar x) = [[[x]]]
gymValueTo3D (Gym1D xs)    = [[xs]]
gymValueTo3D (Gym2D xs)    = [xs]
gymValueTo3D (Gym3D xs)    = xs
gymValueTo3D (GymTuple xs) = map gymValueTo2D xs


fromBool :: Num a => Bool -> a
fromBool True  = 1
fromBool False = 0
