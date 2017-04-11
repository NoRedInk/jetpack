{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import CliArguments (Args (..))
import Config ()
import Control.Monad.Free (foldFree)
import Control.Monad.State
import Data.List as L
import Data.List.Utils (uniq)
import Data.Tree as Tree
import qualified Error
import qualified Interpreter.Pipeline as PipelineI
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
      putStrLn "Compilation failed!"
      System.Exit.die $ L.unlines $ fmap Error.description err
    Right _ -> putStrLn "Compilation succeeded!"

program :: Pipeline ()
program = do
  args        <- readCliArgs
  config      <- readConfig (configPath args)
  toolPaths   <- setup config
  _           <- clearLog config
  entryPoints <- findEntryPoints config args

  _     <- startProgress "Finding dependencies for entrypoints" $ L.length entryPoints
  cache <- readDependencyCache config
  deps  <- async $ fmap (findDependency config cache) entryPoints
  _     <- writeDependencyCache config deps
  _     <- endProgress

  let modules = uniq $ concatMap Tree.flatten deps

  _ <- startProgress "Compiling" $ L.length modules
  logOutput <- traverse (compile config toolPaths) modules
  _ <- traverse (appendLog config) logOutput
  _ <- endProgress

  _       <- startProgress "Write modules" $ L.length deps
  modules <- async $ fmap (concatModule config) deps
  _       <- outputCreatedModules config modules
  _       <- endProgress
  return ()

runProgram :: Pipeline a -> Task a
runProgram = foldFree PipelineI.interpreter
