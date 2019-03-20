{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module ML.Gym.Range
  ( GymRange(..)
  , combineRanges
  , getGymRange
  , gymRangeToDoubleLists
  ) where

import           ML.Gym.DType
import           ML.Gym.Util
import           ML.Gym.Value

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

data GymRange =
  GymRange GymDType
           (GymValue Double)    -- ^ Minimum values.
           (GymValue Double)    -- ^ Maximum values.
  deriving (Show, Eq)


gymRangeToDoubleLists :: GymRange -> ([Double],[Double])
gymRangeToDoubleLists (GymRange _ los his) = (gymValueTo1D los, gymValueTo1D his)

gymRangeTypePrecedence :: GymRange -> Int
gymRangeTypePrecedence (GymRange dt _ _) = case dt of
  GymBool    -> 1
  GymInteger -> 2
  GymFloat   -> 3
  _          -> error $ "range type " ++ show dt ++ " not allowed"

gymRangeTypeFromPrecedence :: Int -> GymDType
gymRangeTypeFromPrecedence 1 = GymBool
gymRangeTypeFromPrecedence 2 = GymInteger
gymRangeTypeFromPrecedence 3 = GymFloat
gymRangeTypeFromPrecedence nr = error $ "gym Range with precedence " ++ show nr ++ " is unknown"

combineRanges :: [GymRange] -> GymRange
combineRanges [] = error "empty list ranges in maxRange"
-- combineRanges xs = uncurry (GymRange (gymRangeTypeFromPrecedence $ maximum (map gymRangeTypePrecedence xs))) (foldl1 mkLoHiRanges (map gymRangeToDoubleLists xs))
--   where mkLoHiRanges (los,his) (los', his') | length los' > length los = mkLoHiRanges (los ++ drop (length los) los', his ++ drop (length his) his') (los', his')
--                                             | length los > length los' = mkLoHiRanges (los,his) (los' ++ drop (length los') los, his' ++ drop (length his') his)
--                                             | otherwise = (zipWith min los los', zipWith max his his')

getGymRange :: Py.Object space => GymDType -> space -> IO (Maybe GymRange)
getGymRange tp@GymBool    = getGymRange' pyToBool fromBool (GymRange tp)
getGymRange tp@GymInteger = getGymRange' pyToInteger fromIntegral (GymRange tp)
getGymRange tp@GymFloat   = getGymRange' pyToDouble id (GymRange tp)
getGymRange tp@GymObject  = getGymRange' pyToDouble id (GymRange tp)
getGymRange tp            = error $ "cannot make range of type " ++ show tp

numPyArray :: Py.SomeObject -> IO Py.SomeObject
numPyArray obj = python $ Py.callMethodArgs obj "tolist" []

getGymRange' :: Py.Object space => (Py.SomeObject -> IO (Maybe a)) -> (a -> Double) -> (GymValue Double -> GymValue Double -> GymRange) -> space -> IO (Maybe GymRange)
getGymRange' convert mkGymVal gymRange space = do
  simple <- getLoHg (convert >=> return . fmap (GymScalar . mkGymVal)) gymRange space
  list <- getLoHg (\x -> fmap Gym1D <$> (numPyArray x >>= pyToListOf (fmap (fmap mkGymVal) . convert))) gymRange space
  ll <- getLoHg (\x -> fmap Gym2D <$> (numPyArray x >>= pyToListOfListOf (fmap (fmap mkGymVal) . convert))) gymRange space
  lll <- getLoHg (\x -> fmap Gym3D <$> (numPyArray x >>= pyToListOfListOfListOf (fmap (fmap mkGymVal) . convert))) gymRange space
  return $ simple <|> list <|> ll <|> lll
  where
    getLoHg convert gymRange space = do
      lo <- lowerBound space >>= convert
      hg <- upperBound space >>= convert
      return $ pure gymRange <*> lo <*> hg

mkSpaceRepr :: Py.Object self => self -> IO T.Text
mkSpaceRepr space = fromMaybe (error "could not get gym action space") <$> python (Py.callMethodArgs space "__repr__" [] >>= pyToText)


lowerBound :: Py.Object space => space -> IO Py.SomeObject
lowerBound space = python (Py.toUnicode "low" >>= Py.getAttribute space)

upperBound :: Py.Object space => space -> IO Py.SomeObject
upperBound space = python (Py.toUnicode "high" >>= Py.getAttribute space)
