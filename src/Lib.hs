{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import CliArguments (Args (..))
import Config ()
import Control.Monad.Except
import Control.Monad.Free (foldFree)
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
  e <- displayConsoleRegions $ runExceptT $ runProgram program
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

  pg   <- startProgress "Finding dependencies for entrypoints" $ L.length entryPoints
  deps <- dependencies pg config entryPoints
  _    <- endProgress pg

  let modules = uniq $ concatMap Tree.flatten deps

  pg <- startProgress "Compiling" $ L.length modules
  logOutput <- async $ fmap (compile pg config toolPaths) modules
  _ <- traverse (appendLog config) logOutput
  _ <- endProgress pg

  pg      <- startProgress "Write modules" $ L.length deps
  modules <- async $ fmap (concatModule config) deps
  _       <- outputCreatedModules pg config modules
  _       <- endProgress pg
  return ()

runProgram :: Pipeline a -> Task a
runProgram = foldFree PipelineI.interpreter
