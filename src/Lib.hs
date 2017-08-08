{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import CliArguments (Args (..))
import Config (Config (..))
import Control.Monad.Free (foldFree)
import Data.List as L
import Data.List.Utils (uniq)
import qualified Data.Text as T
import Data.Tree as Tree
import qualified Error
import qualified Interpreter.Pipeline as PipelineI
import Pipeline
import Rainbow
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
      putChunkLn errorMessage
      System.Exit.die $ L.unlines $ fmap Error.description err
    Right _ -> putChunkLn successMessage

program :: Pipeline ()
program = do
  -- SETUP
  args        <- readCliArgs
  Config {log_directory} <- readConfig (configPath args)
  toolPaths   <- setup
  _           <- clearLog "compile.log"
  _           <- clearLog "pre-hook.log"
  _           <- clearLog "post-hook.log"

  -- HOOKS
  let preHookTitle = (T.pack $ "Pre hook (see " ++ log_directory ++ "/pre-hook.log)" )
  _       <- startSpinner preHookTitle
  _ <- case CliArguments.preHook args of
         Just pathToScript -> do
           hookOutput <- hook pathToScript
           _ <- appendLog "pre-hook.log" hookOutput
           endSpinner
         Nothing   -> endProgress

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

  -- HOOKS
  let postHookTitle = (T.pack $ "Post hook (see " ++ log_directory ++ "/post-hook.log)" )
  _       <- startSpinner postHookTitle
  case CliArguments.postHook args of
    Just pathToScript -> do
      hookOutput <- hook pathToScript
      _ <- appendLog "post-hook.log" hookOutput
      endSpinner
    Nothing   -> endProgress

runProgram :: Pipeline a -> Task a
runProgram = foldFree PipelineI.interpreter


successMessage :: Chunk T.Text
successMessage =
  fore green
    $ chunk
    $ T.unlines
      [ T.replicate 27 "*"
      , "~*~Compilation Succeeded~*~"
      , T.replicate 27 "*"
      ]


errorMessage :: Chunk T.Text
errorMessage =
  fore red
    $ chunk
    $ T.unlines
      [ T.replicate 20 "~"
      , "¡Compilation failed!"
      , " "
      , "    ¯\\_(ツ)_/¯    "
      , T.replicate 20 "~"
      ]
