{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import qualified CliArguments
import Control.Monad.Free (foldFree)
import qualified Data.List as L
import Data.List.Utils (uniq)
import qualified Data.Text as T
import qualified Data.Tree as Tree
import qualified Interpreter.Pipeline as PipelineI
import qualified Message
import Pipeline
import System.Console.AsciiProgress
import qualified System.Exit
import Task

run :: IO ()
run = do
  e <- displayConsoleRegions
    $ runTask
    $ runProgram program
  case e of
    Left err -> do
      _ <- Message.error err
      System.Exit.exitFailure
    Right _ -> do
      Message.success

program :: Pipeline ()
program = do
  -- SETUP
  args      <- readCliArgs
  _         <- readConfig (CliArguments.configPath args)
  toolPaths <- setup
  _         <- traverse clearLog ["compile.log", "pre-hook.log", "post-hook.log"]

  -- HOOKS
  maybeRunHook "pre" (CliArguments.preHook args)

  entryPoints <- findEntryPoints

  -- GETTING DEPENDENCY TREE
  _     <- startProgress "Finding dependencies for entrypoints" $ L.length entryPoints
  cache <- readDependencyCache
  deps  <- async $ fmap (findDependency cache) entryPoints
  _     <- writeDependencyCache deps
  _     <- endProgress

  -- COMPILATION
  let modules = uniq $ concatMap Tree.flatten deps
  _ <- startProgress "Compiling" $ L.length modules
  logOutput <- traverse (compile toolPaths) modules
  _ <- traverse (appendLog "compile.log") logOutput
  _ <- endProgress

  _       <- startProgress "Write modules" $ L.length deps
  modules <- async $ fmap concatModule deps
  _       <- outputCreatedModules modules
  _       <- endProgress

  maybeRunHook "post" (CliArguments.postHook args)

maybeRunHook :: String -> Maybe FilePath -> Pipeline ()
maybeRunHook _ Nothing  = return ()
maybeRunHook name (Just pathToScript) = do
  let title = (T.pack $ name ++ " hook (" ++ pathToScript ++ ")" )
  _ <- startSpinner title
  hookOutput <- hook pathToScript
  appendLog (T.pack $ name ++ "-hook.log") hookOutput
  endSpinner title

runProgram :: Pipeline a -> Task a
runProgram = foldFree PipelineI.interpreter
