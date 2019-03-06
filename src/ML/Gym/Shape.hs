{-# LANGUAGE OverloadedStrings #-}

module ML.Gym.Shape
  ( GymShape (..)
  , getGymShape
  ) where

import           ML.Gym.DType
import           ML.Gym.Util

import qualified Control.Exception        as E
import           Control.Monad            (join, void)
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


data GymShape = Gym1D Integer
              | Gym2D Integer Integer
              deriving (Show, Eq)


getGymShape :: Py.Object space => space -> IO (Maybe GymShape)
getGymShape space = do
  let err = error "could not get gym shape"
  sh <- shape space >>= pyToTupleList >>= fmap (join . fmap sequence) . traverse (mapM pyToInteger)
  return $ mkGymShape <$> (sh :: Maybe [Integer])
  where mkGymShape [x1]     = Gym1D x1
        mkGymShape [x1, x2] = Gym2D x1 x2
        mkGymShape xs       = error $ "Gym shape '" ++ show xs ++ "' not supported yet!"

shape :: Py.Object space => space -> IO Py.SomeObject
shape space = python (Py.toUnicode "shape" >>= Py.getAttribute space)
