module Main where

import qualified Config
import Control.Concurrent
import Control.Monad.Except (throwError)
import Data.Foldable (for_)
import qualified Data.Text as T
import Error
import GHC.IO.Handle
import qualified Lib
import qualified System.Directory as Dir
import System.Environment
import System.Exit
import System.FilePath ()
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process
import Task (Task, toTask)
import qualified Task
import Twitch
       (DebounceType(..), Dep, LoggerType(..), Options(..), addModify,
        defaultMainWithOptions)

main :: IO ()
main = do
  cwd <- Dir.getCurrentDirectory
  maybeConfig <- Task.runTask $ Config.load cwd
  case maybeConfig of
    Right (Just config) -> watch config
    _ -> putStrLn "no jetpack config found."

watch :: Config.Config -> IO ()
watch config = do
  mVar <- newEmptyMVar
  rebuild mVar
  defaultMainWithOptions (options config) $
    for_ fileTypesToWatch $ addModify $ const $ rebuild mVar

fileTypesToWatch :: [Dep]
fileTypesToWatch =
  ["**/*.elm", "**/*.coffee", "**/*.js", "**/*.sass", "**/*.scss", "**/*.json"]

options :: Config.Config -> Options
options config =
  Options
    NoLogger -- log
    Nothing -- logFile
    (Just $ Config.source_directory config) -- root
    True -- recurseThroughDirectories
    Twitch.DebounceDefault -- debounce
    0 -- debounce interval (required but not used)
    0 -- pollInterval
    False -- usePolling

rebuild :: MVar ProcessID -> IO ()
rebuild mVar = do
  runningProcess <- tryTakeMVar mVar
  -- NOTE: there might be a race condition here,
  -- where we didn't add the process handle to the MVar yet.
  for_ runningProcess (signalProcess softwareTermination)
  for_ runningProcess (getProcessStatus True False) -- here be dragons, potentially
  processID <- run
  putMVar mVar processID

run :: IO ProcessID
run = do
  args <- getArgs
  let argsAsString = unwords args
  procId <- forkProcess Lib.run
  return procId
