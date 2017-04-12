{-# LANGUAGE NamedFieldPuns #-}
module Interpreter.Pipeline
  (interpreter
  ) where

import qualified Compile
import ConcatModule
import Config
import qualified Control.Concurrent.Async.Lifted as Concurrent
import Control.Monad.Free (foldFree)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified DependencyTree
import qualified EntryPoints
import qualified Logger
import Algebra.Pipeline
import qualified ProgressBar
import System.FilePath ((<.>), (</>))
import Task (Task, getConfig, toTask)

interpreter :: PipelineF a -> Task a
interpreter command =
  case command of
    ReadDependencyCache next             -> next <$> DependencyTree.readTreeCache
    WriteDependencyCache deps next       -> DependencyTree.writeTreeCache deps >> return next
    FindEntryPoints args next            -> next <$> EntryPoints.find args
    FindDependency cache entryPoint next -> next <$> DependencyTree.build cache entryPoint
    Compile toolPaths dep next           -> next <$> Compile.compile toolPaths dep
    ConcatModule dep next                -> next <$> ConcatModule.wrap dep
    OutputCreatedModules paths next      -> createdModulesJson paths >> return next
    StartProgress title total next       -> ProgressBar.start total title >> return next
    EndProgress next                     -> ProgressBar.end >> return next
    AppendLog msg next                   -> Logger.appendLog msg >> return next
    Async commands next                  -> next <$> Concurrent.forConcurrently commands (foldFree interpreter)


createdModulesJson :: [FilePath] -> Task ()
createdModulesJson paths = do
  let encodedPaths = Aeson.encode paths
  Config {temp_directory} <- Task.getConfig
  let jsonPath = temp_directory </> "modules" <.> "json"
  _ <- toTask $ BL.writeFile jsonPath encodedPaths
  _ <- ProgressBar.step
  return ()
