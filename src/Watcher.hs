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
import qualified Notify
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
watch config args
  -- mVar <- newMVar Nothing
 = do
  putStrLn "Watching. Hit any key to exit."
  -- rebuild config args mVar
  -- processID <-
  --   forkProcess $
  --     -- redirect the process stdout and stderr to /dev/null. This probably won't
  --     -- work on Windows, but then nothing here does yet so... ok?
  --    do
  --     defaultMainWithOptions (options config) $
  --       traverse_
  --         (addModify (const $ rebuild config args mVar))
  --         fileTypesToWatch
  -- -- TODO: we're swallowing any errors that occur here. We were before too, but
  -- -- now we *explicitly* are.
  -- getProcessStatus True False processID
  Notify.watch "/Users/stoeffel/nri/noredink" (\_ -> Builder.build config args)
  return ()

fileTypesToWatch :: [Dep]
fileTypesToWatch =
  ["**/*.elm", "**/*.coffee", "**/*.js", "**/*.sass", "**/*.scss", "**/*.json"]
