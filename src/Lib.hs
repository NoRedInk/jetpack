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
import qualified Data.Maybe as M
import qualified Data.Text as T
import Data.Tree as Tree
import qualified Error
import qualified Interpreter.Pipeline as PipelineI
import Pipeline
import Rainbow
import System.Console.AsciiProgress
import qualified System.Console.Terminal.Size as TermSize
import qualified System.Exit
import Task

run :: IO ()
run = do
  e <- displayConsoleRegions
    $ runTask
    $ runProgram program
  termWidth <- max 20 <$> M.maybe 20 TermSize.width <$> TermSize.size
  case e of
    Left err -> do
      putChunkLn (separator termWidth "~" & fore red)
      _ <- putChunkLn
              $ fore brightRed
              $ chunk
              $ L.unlines
              $ fmap Error.description err
      putChunkLn (separator termWidth "~" & fore red)
      putChunkLn (errorMessage termWidth)
      putChunkLn (separator termWidth "~" & fore red)
      System.Exit.die ""
    Right _ -> do
      putChunkLn (separator termWidth "*" & fore green)
      putChunkLn (successMessage termWidth)
      putChunkLn (separator termWidth "*" & fore green)

program :: Pipeline ()
program = do
  -- SETUP
  args        <- readCliArgs
  Config {log_directory} <- readConfig (configPath args)
  toolPaths   <- setup
  _           <- traverse clearLog ["compile.log", "pre-hook.log", "post-hook.log"]

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


successMessage :: Int -> Chunk T.Text
successMessage width =
  fore green $ chunk
    $ center width "~*~Compilation Succeeded~*~"

separator :: Int -> T.Text -> Chunk T.Text
separator width c =
    chunk $ T.replicate width c

errorMessage :: Int -> Chunk T.Text
errorMessage width =
  fore red $ chunk $ T.unlines $
    fmap (center width . T.pack)
      [ "¡Compilation failed!"
      , "¯\\_(ツ)_/¯"
      ]

center :: Int -> T.Text -> T.Text
center width msg =
  T.append (T.replicate n " ") msg
  where
    textLength = T.length msg
    half = quot width 2
    halfText = quot textLength 2
    n = half - halfText
