{-# LANGUAGE OverloadedStrings #-}
module ML.Gym.DType where

import qualified Control.Exception        as E
import           Control.Monad            (join, liftM, void)
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


data GymDType = GymBool | GymInteger | GymFloat | GymObject | GymByteString | GymText | GymVoid
  deriving (Show, Eq)

-------------------- Converting --------------------

pyToInteger :: Py.SomeObject -> IO (Maybe Integer)
pyToInteger = pyToA Py.fromInteger

pyToText :: Py.SomeObject -> IO (Maybe T.Text)
pyToText = pyToA Py.fromUnicode

pyToFloat :: Py.SomeObject -> IO (Maybe Double)
pyToFloat = pyToA Py.fromFloat

pyToDouble :: Py.SomeObject -> IO (Maybe Double)
pyToDouble = pyToFloat

pyToBool :: Py.SomeObject -> IO (Maybe Bool)
pyToBool obj = do
  true <- Py.isTrue obj
  if true
    then return $ Just True
    else do
    false <- Py.isFalse obj
    if false
      then return $ Just False
      else return $ Nothing

pyToVoid :: Py.SomeObject -> IO (Maybe ())
pyToVoid _ = return $ Just ()

pyToComplex :: Py.SomeObject -> IO (Maybe (Complex Double))
pyToComplex = pyToA Py.fromComplex

pyToByteString :: Py.SomeObject -> IO (Maybe BS.ByteString)
pyToByteString = pyToA Py.fromBytes

pyToTupleList :: Py.SomeObject -> IO (Maybe [Py.SomeObject])
pyToTupleList = pyToA Py.fromTuple

pyToList :: Py.SomeObject -> IO (Maybe [Py.SomeObject])
pyToList = pyToA Py.fromList

pyToListOf :: (Py.SomeObject -> IO (Maybe a)) -> Py.SomeObject -> IO (Maybe [a])
pyToListOf convert = fmap join . join . fmap (traverse (fmap sequence . mapM convert)) . pyToList

pyToListOfListOf :: (Py.SomeObject -> IO (Maybe a)) -> Py.SomeObject -> IO (Maybe [[a]])
pyToListOfListOf convert obj = do
  mList <- pyToList obj
  join . fmap sequence <$> sequence (mapM (pyToListOf convert) <$> mList)


pyToListOfListOfListOf :: (Py.SomeObject -> IO (Maybe a)) -> Py.SomeObject -> IO (Maybe [[[a]]])
pyToListOfListOfListOf convert obj = do
  mList <- pyToList obj
  join . fmap sequence <$> sequence (mapM (pyToListOfListOf convert) <$> mList)


pyToA :: (Py.Concrete b) => (b -> IO a) -> Py.SomeObject -> IO (Maybe a)
pyToA convert obj = do
  isNone <- Py.isNone obj
  if isNone
    then return Nothing
    else Py.cast obj >>= traverse convert


getGymDType :: Py.Object self => self -> IO GymDType
getGymDType dType = do
  kind <- T.unpack . fromMaybe (error "gym type 'kind' not recognized!") <$> (Py.toUnicode "kind" >>= Py.getAttribute dType >>= pyToText)
  return $
    case kind of
      "b" -> GymBool       -- boolean
      "i" -> GymInteger    -- signed integer
      "u" -> GymInteger    -- unsigned integer
      "f" -> GymFloat      -- floating-point
      "c" -> GymFloat      -- complex floating-point
      "m" -> GymInteger    -- timedelta
      "M" -> GymInteger    -- datetime
      "O" -> GymObject     -- object
      "S" -> GymByteString -- (byte-)string
      "U" -> GymText       -- Unicode
      "V" -> GymVoid       -- void
      _ -> error $ "ERROR: unknown gym (numpy) data type/kind named '" ++ show kind ++ "'!"

