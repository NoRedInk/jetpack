module Interpreter.Pipeline
  (interpreter
  ) where

import CliArguments (readArguments)
import qualified Compile
import ConcatModule
import qualified Config
import Control.Monad.Trans.Class (lift)
import qualified DependencyTree
import qualified EntryPoints
import qualified Init
import Pipeline
import Task (Task)

interpreter :: PipelineF a -> Task a
interpreter command =
  case command of
    ReadCliArgs next                       -> next <$> lift readArguments
    ReadConfig _ next                      -> lift (putStrLn "TODO") >> return (next Config.defaultConfig)
    FindEntryPoints config args next       -> next <$> EntryPoints.find config args
    Dependencies config entryPoints next          -> next <$> DependencyTree.build config entryPoints
    Compile config toolPaths deps next     -> Compile.compileModules config toolPaths deps >> return next
    Init config next                       -> next <$> Init.setup config
    ConcatModules config dependencies next -> next <$> ConcatModule.wrap config dependencies
