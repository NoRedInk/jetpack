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
import qualified System.FSNotify as FS
import System.FilePath ()
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process
import Task (Task, toTask)
import qualified Task
import Twitch
       (DebounceType(..), Dep, LoggerType(..), Options(..), addModify)
import Twitch.Extra (defaultMainWithOptions)

main :: IO ()
main = do
  cwd <- Dir.getCurrentDirectory
  maybeConfig <- Task.runTask $ Config.load cwd
  case maybeConfig of
    Right (Just config) -> watch config
    _ -> putStrLn "no jetpack config found."

watch :: Config.Config -> IO ()
watch config = do
  mVar <- newMVar Nothing
  putStrLn "Watching. Hit ctrl-c to exit."
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
    Twitch.Debounce
    0.5 -- debounce time, in seconds.
    0 -- pollInterval
    False -- usePolling

rebuild :: MVar (Maybe ProcessID) -> IO ()
rebuild mVar = do
  -- takeMVar blocks if there is nothing inside it.
  -- This prevents a race condition that could happen if multiple files are
  -- written at once.
  runningProcess <- takeMVar mVar
  for_ runningProcess (signalProcess softwareTermination)
  for_ runningProcess (getProcessStatus True False) -- here be dragons, potentially
  processID <- run
  putMVar mVar (Just processID)

run :: IO ProcessID
run = do
  args <- getArgs
  let argsAsString = unwords args
  procId <- forkProcess Lib.run
  return procId
