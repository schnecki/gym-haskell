{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module ML.Gym.Gym
    ( Gym (..)
    , GymResult (..)
    , GymObservation
    , initGym
    , resetGym
    , stepGymRandom
    , stepGymRenderRandom
    , stepGym
    , stepGymRender
    , setMaxEpisodeSteps
    , getMaxEpisodeSteps
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
  { name             :: T.Text
  , gym              :: !GymModule
  , env              :: !GymEnv
  , actionSpace      :: !GymActionSpace
  , observationSpace :: !GymObservationSpace
  }

instance Show Gym where
  show (Gym n _ _ acts obs) = T.unpack n ++  "; Action Space: " ++ show acts ++ "; Observation Space: " ++ show obs

data GymResult = GymResult
  { observation :: !GymObservation
  , reward      :: !Double
  , episodeDone :: !Bool
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
  -- putStrLn $ "Called: " ++ name  ++ " " ++ unwords args
  Py.initialize
  res <- Py.isInitialized
  version <- Py.getVersion
  -- putStrLn $ "Initialized python: " ++ show res ++ "; Version: " ++ show version
  Py.setArgv (T.pack name <> "-" <> envName) (map T.pack args)
  -- putStrLn "Arguments set"
  gGym <- python $ Py.importModule "gym"
  -- putStrLn "Module imported"
  pyName <- Py.toUnicode envName
  -- putStrLn "Calling make"
  gEnv <- python $ Py.callMethodArgs gGym "make" [Py.toObject pyName]
  -- putStrLn "Createing action and observation space make"
  gActSpace <- getActionSpace gEnv
  gObsSpace <- getObservationSpace gEnv
  -- putStrLn "Resetting environment"
  gObservation <- python $ Py.callMethodArgs gEnv "reset" []
  obs <- fromMaybe (error "could not convert observation to GymData") <$> python (getGymData gObsSpace gObservation)
  -- putStrLn "Done"
  return (obs, Gym envName gGym gEnv gActSpace gObsSpace)

-------------------- Functions --------------------

-- | Set the maximum number of steps before an episode is finished.
setMaxEpisodeSteps :: Gym -> Integer -> IO ()
setMaxEpisodeSteps gym val = do
  pyVal <- Py.toInteger val
  python $ Py.toUnicode "_max_episode_steps" >>= \attr -> Py.setAttribute (env gym) attr (Py.toObject pyVal)

-- | Get the maximum number of steps before an episode is finished.
getMaxEpisodeSteps :: Gym -> IO (Maybe Integer)
getMaxEpisodeSteps gym = python $ Py.toUnicode "_max_episode_steps" >>= Py.getAttribute (env gym) >>= pyToInteger

-- | Get the number of elapsed steps in this episode.
getElapsedSteps :: Gym -> IO (Maybe Integer)
getElapsedSteps gym = python $ Py.toUnicode "_elapsed_steps" >>= Py.getAttribute (env gym) >>= pyToInteger

-- | Reset the environment.
resetGym :: Gym -> IO GymObservation
resetGym gym = do
  gObs <- python $ Py.callMethodArgs (env gym) "reset" []
  fromMaybe (error "could not convert observation to GymData") <$> python (getGymData (observationSpace gym) gObs)

-- | Take a random step and renders the environment. Also see @stepGymRenderRandom@.
stepGymRandom :: Gym -> IO GymResult
stepGymRandom = stepGymRenderRandom True

-- | Take a random step and specify wether to render the environment or not.
stepGymRenderRandom :: Bool -> Gym -> IO GymResult
stepGymRenderRandom render gym = do
  idx <- randomRIO (0, spaceSize (actionSpace gym) - 1)
  stepGymRender render gym idx

-- | Take a step as specified by the given action index. Renders the environment. See also @stepGymRender@.
stepGym :: Gym -> Integer -> IO GymResult
stepGym = stepGymRender True

-- | Take a step as specified by the given action index. Choose wether to render the environment.
stepGymRender :: Bool -> Gym -> Integer -> IO GymResult
stepGymRender render gym actIdx = do
  when render $ void $ python $ Py.callMethodArgs (env gym) "render" []
  act <- toAction (actionSpace gym) actIdx
  Just tuple <- python $ Py.callMethodArgs (env gym) "step" [Py.toObject act] >>= Py.cast
  [gObs, gReward, gDone, gInfo] <- python $ Py.fromTuple tuple
  obs <- fromMaybe (error "could not convert observation to GymData") <$> python (getGymData (observationSpace gym) gObs)
  rew <- fromMaybe (error "could not convert reward to Double") <$> python (pyToDouble gReward)
  done <- fromMaybe (error "could not convert done falg to Bool") <$> python (pyToBool gDone)
  -- Py.print gDone
  -- void $ python $ Py.print reward stdout
  -- void $ python $ Py.print gDone stdout
  -- void $ python $ Py.print info stdout
  return $ GymResult obs rew done


