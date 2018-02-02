module Interpreter.Pipeline
  ( interpreter
  ) where

import CliArguments (readArguments)
import qualified Compile
import ConcatModule
import Config
import qualified Control.Concurrent.Async.Lifted as Concurrent
import Control.Monad.Free (foldFree)
import Data.Semigroup ((<>))
import qualified Version

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified DependencyTree
import qualified EntryPoints
import qualified Hooks
import qualified Init
import qualified Logger
import Pipeline
import qualified ProgressBar
import qualified ProgressSpinner
import System.FilePath ((<.>), (</>))
import Task (Task, getArgs, getConfig, toTask)

interpreter :: PipelineF a -> Task a
interpreter command =
  case command of
    ReadCliArgs next -> next <$> readArguments
    ReadConfig _ next -> next <$> Config.readConfig
    ReadDependencyCache next -> next <$> DependencyTree.readTreeCache
    WriteDependencyCache deps next ->
      DependencyTree.writeTreeCache deps >> return next
    FindEntryPoints next -> next <$> EntryPoints.find
    FindDependency cache entryPoint next ->
      next <$> DependencyTree.build cache entryPoint
    Compile toolPaths dep next -> next <$> Compile.compile toolPaths dep
    Init next -> next <$> Init.setup
    ConcatModule dep next -> next <$> ConcatModule.wrap dep
    OutputCreatedModules paths next -> createdModulesJson paths >> return next
    StartProgress title total next ->
      ProgressBar.start total title >> return next
    EndProgress next -> ProgressBar.end >> return next
    StartSpinner title next -> ProgressSpinner.start title >> return next
    EndSpinner title next -> ProgressSpinner.end title >> return next
    AppendLog fileName msg next -> Logger.appendLog fileName msg >> return next
    ClearLog fileName next -> Logger.clearLog fileName >> return next
    Hook hookScript next -> next <$> Hooks.run hookScript
    Version next -> next <$> (return Version.print :: Task T.Text)
    Time fileName duration next -> printTime fileName duration >> return next
    Async commands next ->
      next <$> Concurrent.forConcurrently commands (foldFree interpreter)

printTime :: FilePath -> Compile.Duration -> Task ()
printTime path duration = do
  Args {time} <- Task.getArgs
  if time
    then do
      toTask $
        putStrLn $ T.unpack $ (T.pack path) <> ": " <> (T.pack $ show duration)
    else return ()

createdModulesJson :: [FilePath] -> Task ()
createdModulesJson paths = do
  let encodedPaths = Aeson.encode paths
  Config {temp_directory} <- Task.getConfig
  let jsonPath = temp_directory </> "modules" <.> "json"
  _ <- toTask $ BL.writeFile jsonPath encodedPaths
  _ <- ProgressBar.step
  return ()
