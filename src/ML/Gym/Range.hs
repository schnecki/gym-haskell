{-# LANGUAGE OverloadedStrings #-}
module ML.Gym.Range
  ( GymRange(..)
  , getGymRange
  , gymRangeToDoubleLists
  , combineRanges
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

gymRangeToDoubleLists :: GymRange -> ([Double],[Double])
gymRangeToDoubleLists (GymDoubleRange lo hi) = ([lo], [hi])
gymRangeToDoubleLists (GymIntegerRange lo hi) = ([fromIntegral lo], [fromIntegral hi])
gymRangeToDoubleLists GymBoolRange = ([0], [1])
gymRangeToDoubleLists (GymDoubleArrayRange los his) = (los, his)
gymRangeToDoubleLists (GymIntegerArrayRange los his) = (map fromIntegral los, map fromIntegral his)
gymRangeToDoubleLists (GymBoolArrayRange nr) = (replicate nr 0, replicate nr 1)

gymRangeFromDouleList :: Int -> ([Double], [Double]) -> GymRange
gymRangeFromDouleList 1  (los,his)        = GymDoubleArrayRange  los his
gymRangeFromDouleList 2       ([lo],[hi]) = GymDoubleRange lo hi
gymRangeFromDouleList 3 (los, his)        = GymIntegerArrayRange (map round los) (map round his)
gymRangeFromDouleList 4      ([lo], [hi]) = GymIntegerRange (round lo) (round hi)
gymRangeFromDouleList 5    (xs,_)         = GymBoolArrayRange (length xs)
gymRangeFromDouleList 6          _        = GymBoolRange
gymRangeFromDouleList nr vals = error $ "cannot create GymRange in gymRangeFromDouleList with parameters " ++ show nr ++ " and " ++ show vals


gymRangeTypePrecedence :: GymRange -> Int
gymRangeTypePrecedence GymDoubleArrayRange{}  = 1
gymRangeTypePrecedence GymDoubleRange {}      = 2
gymRangeTypePrecedence GymIntegerArrayRange{} = 3
gymRangeTypePrecedence GymIntegerRange{}      = 4
gymRangeTypePrecedence GymBoolArrayRange{}    = 5
gymRangeTypePrecedence GymBoolRange{}         = 6


combineRanges :: [GymRange] -> GymRange
combineRanges [] = error "empty list ranges in maxRange"
combineRanges xs = gymRangeFromDouleList (minimum (map gymRangeTypePrecedence xs)) (foldl1 mkLoHiRanges (map gymRangeToDoubleLists xs))
  where mkLoHiRanges (los,his) (los', his') | length los' > length los = mkLoHiRanges (los ++ drop (length los) los', his ++ drop (length his) his') (los', his')
                                            | length los > length los' = mkLoHiRanges (los,his) (los' ++ drop (length los') los, his' ++ drop (length his') his)
                                            | otherwise = (zipWith min los los', zipWith max his his')

getGymRange :: Py.Object space => GymDType -> space -> IO (Maybe GymRange)
getGymRange  GymBool    = getGymRange' pyToBool (const . const $ GymBoolRange) (\xs _ -> GymBoolArrayRange (length xs))
getGymRange  GymInteger = getGymRange' pyToInteger GymIntegerRange GymIntegerArrayRange
getGymRange  GymFloat   = getGymRange' pyToFloat GymDoubleRange GymDoubleArrayRange
getGymRange  GymObject  = getGymRange' pyToFloat GymDoubleRange GymDoubleArrayRange
getGymRange tp          = error $ "cannot make range of type " ++ show tp

numPyArray :: Py.SomeObject -> IO Py.SomeObject
numPyArray obj = python $ Py.callMethodArgs obj "tolist" []

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
