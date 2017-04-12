{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import CliArguments (Args (..))
import Config ()
import Control.Monad.Free (Free, foldFree)
import Data.Functor.Sum
import Data.List as L
import Data.List.Utils (uniq)
import Data.Tree as Tree
import qualified Error
import qualified Interpreter.Pipeline as PipelineI
import qualified Interpreter.Setup as SetupI
import Algebra.Pipeline
import Algebra.Setup
import System.Console.AsciiProgress
import qualified System.Exit
import Task
import qualified Utils.Free

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

program :: Program ()
program = do
  -- SETUP
  (args, toolPaths) <- Utils.Free.toLeft $ do
    args        <- readCliArgs
    _           <- readConfig (configPath args)
    toolPaths   <- setup
    _           <- clearLog
    pure (args, toolPaths)
  Utils.Free.toRight $ do
    entryPoints <- findEntryPoints args

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

runProgram :: Program a -> Task a
runProgram = foldFree go
  where
  go :: ProgramF a -> Task a
  go (InL s) = SetupI.interpreter s
  go (InR p) = PipelineI.interpreter p

type ProgramF = Sum SetupF PipelineF
type Program = Free ProgramF
