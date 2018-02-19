module Watcher where

import qualified Builder
import CliArguments (Args(..))
import Config
import Control.Concurrent
import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Error
import GHC.IO.Handle
import qualified Message
import qualified Notify
import Notify (Config(..))
import qualified System.Directory as Dir
import System.Environment
import System.Exit
import System.FilePath ()
import qualified System.Posix.IO as PIO
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process

watch :: Config.Config -> Args -> IO ()
watch config args = do
  putStrLn "Watching. Hit `ctrl-c` to exit."
  Notify.watch
    Notify.Config
    { pathToWatch = Config.source_directory config
    , relevantExtensions = [".elm", ".coffee", ".js", ".sass", ".scss", ".json"]
    , debounceInSecs = 0
    , onChange = Builder.build config args
    , onError = \msg -> Message.printError [msg]
    }
