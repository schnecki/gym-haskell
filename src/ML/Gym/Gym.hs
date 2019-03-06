{-# LANGUAGE OverloadedStrings #-}
module ML.Gym.Gym
    ( Gym (..)
    , initGym
    , resetGym
    , stepGymRandom
    , stepGym
    ) where

import           ML.Gym.DType
import           ML.Gym.Range
import           ML.Gym.Shape
import           ML.Gym.Space
import           ML.Gym.Util

import qualified Control.Exception        as E
import           Control.Monad            (void)
import qualified CPython                  as Py
import qualified CPython.Constants        as Py
import qualified CPython.Protocols.Object as Py
import qualified CPython.Types            as Py
import qualified CPython.Types.Exception  as Py
import qualified CPython.Types.Module     as Py
import qualified CPython.Types.Module     as Py
import qualified Data.ByteString          as BS
import           Data.Complex             (Complex)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import           System.Environment       (getArgs, getProgName)
import           System.Exit
import           System.IO
import           System.Random            (randomRIO)


type GymEnv = Py.SomeObject
type GymModule = Py.Module
type GymObservation = Py.SomeObject
type GymActionSpace = GymSpace
type GymObservationSpace = GymSpace


data Gym = Gym
  { gym              :: GymModule
  , env              :: GymEnv
  , actionSpace      :: GymActionSpace
  , observationSpace :: GymObservationSpace
  }


-------------------- Init --------------------

getActionSpace :: GymEnv -> IO GymActionSpace
getActionSpace gEnv = do
  actSpace <- python $ Py.getAttribute gEnv =<< Py.toUnicode "action_space"
  jsonActionSpace <- fromMaybe (error "could not get gym action space") <$> python (Py.callMethodArgs actSpace "__repr__" [] >>= pyToText)
  toGymSpace actSpace jsonActionSpace

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

getObservationSpace :: GymEnv -> IO GymObservationSpace
getObservationSpace gEnv = do
  obsSpace <- python $ Py.getAttribute gEnv =<< Py.toUnicode "observation_space"
  jsonObservationSpace <- fromMaybe (error "could not get gym observation_space") <$> python (Py.callMethodArgs obsSpace "__repr__" [] >>= pyToText)
  toGymSpace obsSpace jsonObservationSpace

-- | Initializes the gym environment. It also resets the environment, s.t. `stepGym` can be called immediately.
initGym :: IO (GymObservation, Gym)
initGym = do
  name <- getProgName
  args <- getArgs

  Py.initialize
  Py.setArgv (T.pack $ name ++ "-gym") (map T.pack args)
  gGym <- python $ Py.importModule "gym"
  pyName <- Py.toUnicode "CartPole-v0"
  gEnv <- python $ Py.callMethodArgs gGym "make" [Py.toObject pyName]
  gObservation <- python $ Py.callMethodArgs gEnv "reset" []
  gActSpace <- getActionSpace gEnv
  gObsSpace <- getObservationSpace gEnv
  return (gObservation, Gym gGym gEnv gActSpace gObsSpace)

resetGym :: Gym -> IO GymObservation
resetGym gym = python $ Py.callMethodArgs (env gym) "reset" []

stepGymRandom :: Gym -> IO GymResult
stepGymRandom gym = do
  idx <- randomRIO (0, actionCount (actionSpace gym) - 1)
  stepGym gym idx

data GymResult = GymResult
  { observation :: GymObservation
  }

stepGym :: Gym -> Integer -> IO GymResult
stepGym gym actIdx = do
  void $ python $ Py.callMethodArgs (env gym) "render" []
  act <- python $ Py.toInteger actIdx
  actionSpace <- Py.getAttribute (env gym) =<< Py.toUnicode "action_space"
  -- sample <- python $ Py.callMethodArgs actionSpace "sample" []
  -- python $ Py.print sample stdout
  -- Just tuple <- python $ Py.callMethodArgs (env gym) "step" [Py.toObject sample] >>= Py.cast
  Py.print act stdout
  Just tuple <- python $ Py.callMethodArgs (env gym) "step" [Py.toObject act] >>= Py.cast
  [obs, reward, done, info] <- python $ Py.fromTuple tuple
  void $ python $ Py.print obs stdout
  -- void $ python $ Py.print reward stdout
  -- void $ python $ Py.print done stdout
  -- void $ python $ Py.print info stdout
  return $ GymResult obs
