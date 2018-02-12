module Lib
  ( run
  ) where

import qualified Builder
import CliArguments (Args(..), readArguments)
import qualified CliArguments
import qualified Compile
import ConcatModule
import Config
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async.Lifted as Concurrent
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.List.Utils as LU
import qualified Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Tree as Tree
import qualified DependencyTree
import qualified EntryPoints
import qualified Hooks
import qualified Init
import qualified Logger
import qualified Message
import ProgressBar (ProgressBar, complete, start, tick)
import qualified ProgressSpinner
import qualified System.Console.AsciiProgress as AsciiProgress
import qualified System.Exit
import System.FilePath ((<.>), (</>))
import System.Process
import Task (Task, lift)
import qualified Task
import qualified Version
import qualified Watcher

run :: IO ()
run
  -- SETUP
 = do
  config <- Config.readConfig
  args <- readArguments
  if CliArguments.version args
    then Message.info Version.print
    else if CliArguments.watch args
           then Watcher.watch config args
           else Builder.build config args
