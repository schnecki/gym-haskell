{-# LANGUAGE OverloadedStrings #-}
module ML.Gym.Data
  ( GymData(..)
  , getGymData
  , gymObservationToDoubleList
  , gymObservationToFloatList
  ) where

import           ML.Gym.DType
import           ML.Gym.Range
import           ML.Gym.Space
import           ML.Gym.Util
import           ML.Gym.Value

import           Control.Applicative
import qualified Control.Exception        as E
import           Control.Monad            (void, zipWithM, (>=>))
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

type GymData = GymValue Double


gymObservationToDoubleList :: GymData -> [Double]
gymObservationToDoubleList = gymValueTo1D

gymObservationToFloatList :: GymData -> [Float]
gymObservationToFloatList = map realToFrac . gymObservationToDoubleList


getGymData :: GymSpace -> Py.SomeObject -> IO (Maybe GymData)
getGymData (GymSpaceDiscrete nr) obj  = fmap (GymScalar . fromIntegral) <$> pyToInteger obj
getGymData (GymSpaceBox range@(GymRange dt _ _) _) obj  = getGymValueWithRange range obj
getGymData (GymSpaceTuple spaces) obj = do
  list <- fromMaybe (error "Gym discrete space 'n' not recognized!") <$> python (Py.toUnicode "spaces" >>= Py.getAttribute obj >>= pyToList)
  fmap GymTuple . sequence <$> zipWithM getGymData spaces list

fromScalar :: GymDType -> Py.SomeObject -> IO (Maybe (GymValue Double))
fromScalar GymBool obj    = fmap (GymScalar . fromBool) <$> pyToBool obj
fromScalar GymInteger obj = fmap (GymScalar . fromIntegral) <$> pyToInteger obj
fromScalar GymFloat obj   = fmap GymScalar <$> pyToDouble obj
fromScalar dt _           = error $ "missing implemenations in fromScalar: " ++ show dt

from1D :: GymDType -> Py.SomeObject -> IO (Maybe (GymValue Double))
from1D GymBool obj    = fmap (Gym1D . map fromBool) <$> pyToListOf pyToBool obj
from1D GymInteger obj = fmap (Gym1D . map fromIntegral) <$> pyToListOf pyToInteger obj
from1D GymFloat obj   = fmap Gym1D <$> pyToListOf pyToDouble obj
from1D dt _           = error $ "missing implemenations in fromScalar: " ++ show dt

from2D :: GymDType -> Py.SomeObject -> IO (Maybe (GymValue Double))
from2D GymBool obj    = fmap (Gym2D . map (map fromBool)) <$> pyToListOfListOf pyToBool obj
from2D GymInteger obj = fmap (Gym2D . map (map fromIntegral)) <$> pyToListOfListOf  pyToInteger obj
from2D GymFloat obj   = fmap Gym2D <$> pyToListOfListOf pyToDouble obj
from2D dt _           = error $ "missing implemenations in fromScalar: " ++ show dt

from3D :: GymDType -> Py.SomeObject -> IO (Maybe (GymValue Double))
from3D GymBool obj    = fmap (Gym3D . map (map (map fromBool))) <$> pyToListOfListOfListOf pyToBool obj
from3D GymInteger obj = fmap (Gym3D . map (map (map fromIntegral))) <$> pyToListOfListOfListOf  pyToInteger obj
from3D GymFloat obj   = fmap Gym3D <$> pyToListOfListOfListOf pyToDouble obj
from3D dt _           = error ("missing implementations in fromScalar: " ++ show dt)


getGymValueWithRange :: GymRange -> Py.SomeObject -> IO (Maybe (GymValue Double))
getGymValueWithRange (GymRange dt (GymScalar _) _) obj = fromScalar dt obj
getGymValueWithRange (GymRange dt (Gym1D _) _) obj     = numPyArray obj >>= from1D dt
getGymValueWithRange (GymRange dt (Gym2D _) _) obj     = numPyArray obj >>= from2D dt
getGymValueWithRange (GymRange dt (Gym3D _) _) obj     = numPyArray obj >>= from3D dt
getGymValueWithRange (GymRange dt (GymTuple xs) _) obj = do
  list <- fromMaybe (error "Gym discrete space 'n' not recognized!") <$> python (Py.toUnicode "spaces" >>= Py.getAttribute obj >>= pyToList)
  fmap GymTuple . sequence <$> zipWithM (\v o -> getGymValueWithRange (GymRange dt v (error "unused")) o) xs list

numPyArray :: Py.SomeObject -> IO Py.SomeObject
numPyArray obj = python $ Py.callMethodArgs obj "tolist" []

