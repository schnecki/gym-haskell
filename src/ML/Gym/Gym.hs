{-# LANGUAGE OverloadedStrings #-}
module ML.Gym.Gym
    ( Gym (..)
    , GymResult (..)
    , initGym
    , resetGym
    , stepGymRandom
    , stepGym
    , setMaxEpisodeSteps
    ) where

import           ML.Gym.Data
import           ML.Gym.DType
import           ML.Gym.Range
import           ML.Gym.Shape
import           ML.Gym.Space
import           ML.Gym.Util

import qualified Control.Exception        as E
import           Control.Monad            (void, when)
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


type GymEnv = Py.SomeObject
type GymModule = Py.Module
type GymObservation = GymData
type GymActionSpace = GymSpace
type GymObservationSpace = GymSpace


data Gym = Gym
  { gym              :: GymModule
  , env              :: GymEnv
  , actionSpace      :: GymActionSpace
  , observationSpace :: GymObservationSpace
  }

data GymResult = GymResult
  { observation :: GymObservation
  , reward      :: Double
  , episodeDone :: Bool
  } deriving (Show, Eq)


-------------------- Init --------------------

getActionSpace :: GymEnv -> IO GymActionSpace
getActionSpace gEnv = do
  actSpace <- python $ Py.getAttribute gEnv =<< Py.toUnicode "action_space"
  toGymSpace actSpace

getObservationSpace :: GymEnv -> IO GymObservationSpace
getObservationSpace gEnv = do
  obsSpace <- python $ Py.getAttribute gEnv =<< Py.toUnicode "observation_space"
  toGymSpace obsSpace


-- | Initializes the gym environment. It also resets the environment, s.t. `stepGym` can be called immediately.
initGym :: T.Text -> IO (GymObservation, Gym)
initGym envName = do
  name <- getProgName
  args <- getArgs
  Py.initialize
  Py.setArgv (T.pack name <> "-" <> envName) (map T.pack args)
  gGym <- python $ Py.importModule "gym"
  pyName <- Py.toUnicode envName
  gEnv <- python $ Py.callMethodArgs gGym "make" [Py.toObject pyName]
  gActSpace <- getActionSpace gEnv
  gObsSpace <- getObservationSpace gEnv
  gObservation <- python $ Py.callMethodArgs gEnv "reset" []
  obs <- fromMaybe (error "could not convert observation to GymData") <$> python (getGymData gObsSpace gObservation)
  return (obs, Gym gGym gEnv gActSpace gObsSpace)

-------------------- Functions --------------------

setMaxEpisodeSteps :: Gym -> Integer -> IO ()
setMaxEpisodeSteps gym val = do
  pyVal <- Py.toInteger val
  python $ Py.toUnicode "_max_episode_steps" >>= \attr -> Py.setAttribute (env gym) attr (Py.toObject pyVal)

resetGym :: Gym -> IO GymObservation
resetGym gym = do
  gObs <- python $ Py.callMethodArgs (env gym) "reset" []
  fromMaybe (error "could not convert observation to GymData") <$> python (getGymData (observationSpace gym) gObs)


stepGymRandom :: Gym -> IO GymResult
stepGymRandom gym = do
  idx <- randomRIO (0, spaceSize (actionSpace gym) - 1)
  stepGym gym idx

stepGym :: Gym -> Integer -> IO GymResult
stepGym gym actIdx = do
  void $ python $ Py.callMethodArgs (env gym) "render" []
  act <- toAction (actionSpace gym) actIdx
  Just tuple <- python $ Py.callMethodArgs (env gym) "step" [Py.toObject act] >>= Py.cast
  [gObs, gReward, gDone, gInfo] <- python $ Py.fromTuple tuple
  obs <- fromMaybe (error "could not convert observation to GymData") <$> python (getGymData (observationSpace gym) gObs)
  rew <- fromMaybe (error "could not convert reward to Double") <$> python (pyToDouble gReward)
  done <- fromMaybe (error "could not convert reward to Bool") <$> python (pyToBool gDone)
  -- Py.print gDone
  -- void $ python $ Py.print reward stdout
  -- void $ python $ Py.print gDone stdout
  -- void $ python $ Py.print info stdout
  return $ GymResult obs rew done


  -- actionSpace <- Py.getAttribute (env gym) =<< Py.toUnicode "action_space"
-- sample <- python $ Py.callMethodArgs actionSpace "sample" []
-- python $ Py.print sample stdout
-- Just tuple <- python $ Py.callMethodArgs (env gym) "step" [Py.toObject sample] >>= Py.cast
