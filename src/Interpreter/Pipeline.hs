module Interpreter.Pipeline
  (interpreter
  ) where

import qualified Compile
import ConcatModule
import qualified Config
import Control.Monad.Trans.Class (lift)
import qualified Dependencies
import qualified Init
import Pipeline
import Task (Task)
import CliArguments (defaultArguments, readArguments, Args(..))

interpreter :: PipelineF a -> Task a
interpreter (ReadCliArgs next) = lift (putStrLn "TODO") >> return (next defaultArguments)
interpreter (ReadConfig _ next) = lift (putStrLn "TODO") >> return (next Config.defaultConfig)
interpreter (Dependencies config next) = do
  args <- lift readArguments
  deps <- Dependencies.find config (entryPointGlob args)
  return $ next deps
interpreter (Compile config toolPaths deps next) = do
  Compile.compileModules config toolPaths deps
  return next
interpreter (Init config next) = next <$> Init.setup config
interpreter (ConcatModules config dependencies next) = do
  outputPaths <- ConcatModule.wrap config dependencies
  return (next outputPaths)
