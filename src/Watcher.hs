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
import qualified System.Directory as Dir
import System.Environment
import System.Exit
import System.FilePath ()
import qualified System.Posix.IO as PIO
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process
import Twitch
       (DebounceType(..), Dep, LoggerType(..), Options(..), addModify,
        defaultMainWithOptions)

watch :: Config.Config -> Args -> IO ()
watch config args = do
  mVar <- newMVar Nothing
  putStrLn "Watching. Hit any key to exit."
  rebuild config args mVar
  processID <-
    forkProcess $
      -- redirect the process stdout and stderr to /dev/null. This probably won't
      -- work on Windows, but then nothing here does yet so... ok?
     do
      defaultMainWithOptions (options config) $
        traverse_
          (addModify (const $ rebuild config args mVar))
          fileTypesToWatch
  -- TODO: we're swallowing any errors that occur here. We were before too, but
  -- now we *explicitly* are.
  getProcessStatus True False processID
  return ()

fileTypesToWatch :: [Dep]
fileTypesToWatch =
  ["**/*.elm", "**/*.coffee", "**/*.js", "**/*.sass", "**/*.scss", "**/*.json"]

options :: Config.Config -> Options
options Config {source_directory} =
  Options
    NoLogger -- log
    Nothing -- logFile
    (Just source_directory) -- root
    True -- recurseThroughDirectories
    Twitch.Debounce
    0.5 -- debounce time, in seconds.
    0 -- pollInterval
    False -- usePolling

rebuild :: Config.Config -> Args -> MVar (Maybe ProcessID) -> IO ()
rebuild config args mVar = do
  killCurrentProcess mVar
  processID <- forkProcess $ Builder.build config args
  putMVar mVar (Just processID)

killCurrentProcess :: MVar (Maybe ProcessID) -> IO ()
killCurrentProcess mVar
  -- takeMVar blocks if there is nothing inside it.
  -- This prevents a race condition that could happen if multiple files are
  -- written at once.
 = do
  runningProcess <- takeMVar mVar
  traverse_ (signalProcess softwareTermination) runningProcess
