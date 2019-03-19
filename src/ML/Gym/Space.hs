{-# LANGUAGE OverloadedStrings #-}

module ML.Gym.Space where

import           ML.Gym.DType
import           ML.Gym.Range
import           ML.Gym.Shape
import           ML.Gym.Util
import           ML.Gym.Value

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

import           Debug.Trace


class SpaceSize a where
  spaceSize :: a -> Integer     -- Rename to something better

data GymSpace = Discrete Integer        -- ^ Range: 0...(x-1)
              | Box GymRange GymShape   -- ^ 1D, 2D, or 3D matrix. Range sizes and shape must coincide.
              | Tuple [GymSpace]        -- ^ Tuple of Spaces.
              deriving (Show, Eq)


instance SpaceSize GymSpace where
  spaceSize (Discrete nr)                 = nr
  spaceSize (Box _ (GymShape1D x1))       = x1
  spaceSize (Box _ (GymShape2D x1 x2))    = x1 * x2
  spaceSize (Box _ (GymShape3D x1 x2 x3)) = x1 * x2 * x3
  spaceSize (Tuple spaces)                = product (map spaceSize spaces)


toAction :: GymSpace -> Integer -> IO Py.SomeObject
toAction (Discrete nr) idx
  | idx < 0 || idx > (nr - 1) = error "Action index out of range"
  | otherwise = Py.toObject <$> Py.toInteger idx
toAction (Box _ (GymShape1D x1)) idx = error "Box (probably continuous) actions currently not supported"
toAction (Box _ (GymShape2D x1 x2)) idx = error "Box (probably continuous) actions currently not supported"
toAction (Box _ (GymShape3D x1 x2 x3)) idx = error "Box (probably continuous) actions currently not supported"
toAction (Tuple spaces) idx
  | idx >= fromIntegral (length cats) = error "action index is out of range"
  | otherwise = Py.toObject <$> (mapM (fmap Py.toObject . Py.toInteger) (cats !! fromIntegral idx) >>= Py.toList)
  where
    sizes = map spaceSize spaces
    cats = fromCats sizes


fromCats :: [Integer] -> [[Integer]]
fromCats []     = []
fromCats [x]    = map return [0..x-1]
fromCats (x:xs) = concatMap (\cat -> map (cat ++) (fromCats xs)) (fromCats [x])

fromIdx :: [Integer] -> Integer -> [Integer]
fromIdx cats idx =
  -- trace ("rev: " ++ show (reverse $ scanl1 (*) cats))
  zipWith toCat cats (reverse $ scanl1 (*) cats)
  where toCat i c =
          -- trace ("nr: " ++ show nr)
          -- trace ("c: " ++ show c)
          -- trace ("i: " ++ show i)
          (idx `div` (max 1 (c `div` 2))) `mod` i
        nr = product cats

test :: [Integer] -> Bool
test cats = do

  all (\i ->(fromCats cats !! i) == fromIdx cats (fromIntegral i)) [0..product (map fromIntegral cats)-1]


getGymRangeFromSpace :: GymSpace -> GymRange
getGymRangeFromSpace (Discrete nr)  = GymRange GymInteger (GymScalar 0) (GymScalar $ fromIntegral nr-1)
getGymRangeFromSpace (Box rng _)    = rng
getGymRangeFromSpace (Tuple spaces) =
  trace ("from: " ++ show spaces)
  trace ("to: " ++ show (combineRanges $ map getGymRangeFromSpace spaces))
  combineRanges $ map getGymRangeFromSpace spaces


toGymSpace :: Py.Object self => self -> IO GymSpace
toGymSpace space = mkSpaceRepr >>= toGymSpace'
  where
    mkSpaceRepr = fromMaybe (error "could not get gym action space") <$> python (Py.callMethodArgs space "__repr__" [] >>= pyToText)
    toGymSpace' txt
      | T.isPrefixOf "Discrete" txt = do
        n <- fromMaybe (error "Gym discrete space 'n' not recognized!") <$> python (Py.toUnicode "n" >>= Py.getAttribute space >>= pyToInteger)
        return $ Discrete n
      | T.isPrefixOf "Box" txt = do
        dType <- python $ Py.toUnicode "dtype" >>= Py.getAttribute space >>= getGymDType
        range <- fromMaybe (error $ "could not get gym range: " ++ show dType ++ ", " ++ T.unpack txt) <$> python (getGymRange dType space)
        shape <- fromMaybe (error "could not get gym shape") <$> python (getGymShape space)
        return $ Box range shape
      | T.isPrefixOf "Tuple" txt = do
          list <- fromMaybe (error "Gym discrete space 'n' not recognized!") <$> python (Py.toUnicode "spaces" >>= Py.getAttribute space >>= pyToList)
          spaces <- mapM toGymSpace list
          return $ Tuple spaces
      | otherwise = do
        putStrLn "Sample space: "
        python (Py.callMethodArgs space "sample" [] >>= flip Py.print stdout)
        error $ "ERROR: Gym space '" ++ T.unpack txt ++ "' not supported yet!"
