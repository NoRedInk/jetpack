{-# LANGUAGE NamedFieldPuns #-}
module Interpreter.Pipeline
  (interpreter
  ) where

import CliArguments (readArguments)
import qualified Compile
import ConcatModule
import Config
import qualified Control.Concurrent.Async.Lifted as Concurrent
import Control.Monad.Except
import Control.Monad.Free (foldFree)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Time.Clock
import qualified DependencyTree
import qualified EntryPoints
import Error (Error (..))
import qualified Init
import qualified Logger
import Pipeline
import qualified ProgressBar
import qualified System.Directory as Dir
import System.FilePath ((<.>), (</>))
import Task (Task, getConfig, toTask)

interpreter :: PipelineF a -> Task a
interpreter command =
  case command of
    ReadCliArgs next                     -> next <$> readArguments
    ReadConfig _ next                    -> next <$> Config.readConfig
    ReadDependencyCache next             -> next <$> DependencyTree.readTreeCache
    WriteDependencyCache deps next       -> DependencyTree.writeTreeCache deps >> return next
    FindEntryPoints next                 -> next <$> EntryPoints.find
    FindDependency cache entryPoint next -> next <$> DependencyTree.build cache entryPoint
    Compile toolPaths dep next           -> next <$> Compile.compile toolPaths dep
    WhatNeedsCompilation deps next       -> next <$> Compile.whatNeedsCompilation deps
    Init next                            -> next <$> Init.setup
    ConcatModule dep next                -> next <$> ConcatModule.wrap dep
    OutputCreatedModules paths next      -> createdModulesJson paths >> return next
    StartProgress title total next       -> ProgressBar.start total title >> return next
    EndProgress next                     -> ProgressBar.end >> return next
    AppendLog msg next                   -> Logger.appendLog msg >> return next
    ClearLog next                        -> Logger.clearLog  >> return next
    Async commands next                  -> next <$> Concurrent.forConcurrently commands (foldFree interpreter)

type CompiledModules = Map.Map FilePath UTCTime

createdModulesJson :: [FilePath] -> Task ()
createdModulesJson paths = do
  Config {temp_directory} <- Task.getConfig
  let jsonPath = temp_directory </> "modules" <.> "json"
  compileTime <- toTask getCurrentTime

  exists <- toTask $ Dir.doesFileExist jsonPath
  if exists
    then do
      content <- toTask $ BL.readFile jsonPath
      case Aeson.decode content of
        Just modules -> updateModulesJson jsonPath paths compileTime modules
        Nothing      -> throwError $ [JsonInvalid jsonPath]
    else
      updateModulesJson jsonPath paths compileTime (Map.empty)

updateModulesJson :: FilePath -> [FilePath] -> UTCTime -> CompiledModules -> Task ()
updateModulesJson jsonPath paths compileTime modules = do
  let newContent = fmap (\k -> Map.insert k compileTime modules) paths
  let encodedPaths = Aeson.encode newContent
  _ <- toTask $ BL.writeFile jsonPath encodedPaths
  _ <- ProgressBar.step
  return ()
