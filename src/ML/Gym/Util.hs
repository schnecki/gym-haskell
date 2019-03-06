module ML.Gym.Util
    ( python
    ) where


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

onException :: Py.Exception -> IO a
onException exc = Py.print (Py.exceptionValue exc) stdout >> exitFailure

python :: IO a -> IO a
python = E.handle onException

