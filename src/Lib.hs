{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import CliArguments (Args (..))
import Config ()
import Control.Monad.Free (foldFree)
import Data.List as L
import Data.List.Utils (uniq)
import Data.Tree as Tree
import qualified Data.Text as T
import qualified Error
import qualified Interpreter.Pipeline as PipelineI
import Rainbow
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
      putChunkLn errorMessage
      System.Exit.die $ L.unlines $ fmap Error.description err
    Right _ -> putChunkLn successMessage

program :: Pipeline ()
program = do
  -- SETUP
  args        <- readCliArgs
  _           <- readConfig (configPath args)
  toolPaths   <- setup
  _           <- clearLog
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
  _ <- traverse appendLog logOutput
  _ <- endProgress

  _       <- startProgress "Write modules" $ L.length deps
  modules <- async $ fmap concatModule deps
  _       <- outputCreatedModules modules
  _       <- endProgress
  return ()

runProgram :: Pipeline a -> Task a
runProgram = foldFree PipelineI.interpreter


successMessage :: Chunk T.Text
successMessage =
  fore green
    $ blink
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
