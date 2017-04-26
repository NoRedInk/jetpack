{-# LANGUAGE NamedFieldPuns #-}
module Interpreter.Pipeline
  (interpreter
  ) where

import CliArguments (readArguments)
import qualified Compile
import ConcatModule
import Config
import qualified Control.Concurrent.Async.Lifted as Concurrent
import Control.Monad.Free (foldFree)
import qualified DependencyTree
import qualified EntryPoints
import qualified Init
import qualified Logger
import Pipeline
import qualified ProgressBar
import Task (Task)

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
    OutputCreatedModules deps next       -> Compile.createdModulesJson deps >> return next
    StartProgress title total next       -> ProgressBar.start total title >> return next
    EndProgress next                     -> ProgressBar.end >> return next
    AppendLog msg next                   -> Logger.appendLog msg >> return next
    ClearLog next                        -> Logger.clearLog  >> return next
    Async commands next                  -> next <$> Concurrent.forConcurrently commands (foldFree interpreter)
