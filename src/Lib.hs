{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import qualified CliArguments
import qualified Control.Monad.Free as Free
import qualified Data.List as L
import qualified Data.List.Utils as LU
import qualified Data.Text as T
import qualified Data.Tree as Tree
import qualified Error
import qualified Interpreter.Pipeline as PipelineI
import qualified Logger
import qualified Message
import qualified Pipeline as P
import qualified System.Console.AsciiProgress as AsciiProgress
import qualified System.Exit
import qualified Task

run :: IO ()
run = do
  result <- AsciiProgress.displayConsoleRegions
              $ Task.runTask
              $ Free.foldFree PipelineI.interpreter program
  case result of
    Right _ -> Message.success
    Left err -> do
      _ <- Message.error err
      System.Exit.exitFailure


program :: P.Pipeline ()
program = do
  -- SETUP
  args      <- P.readCliArgs
  _         <- P.readConfig (CliArguments.configPath args)
  toolPaths <- P.setup
  _         <- traverse P.clearLog Logger.allLogs

  -- HOOK
  maybeRunHook "pre" (CliArguments.preHook args)

  entryPoints <- P.findEntryPoints

  -- GETTING DEPENDENCY TREE
  _     <- P.startProgress "Finding dependencies for entrypoints"
             $ L.length entryPoints
  cache <- P.readDependencyCache
  deps  <- P.async $ fmap (P.findDependency cache) entryPoints
  _     <- P.writeDependencyCache deps
  _     <- P.endProgress

  -- COMPILATION
  let modules = LU.uniq $ concatMap Tree.flatten deps
  _   <- P.startProgress "Compiling" $ L.length modules
  out <- traverse (P.compile toolPaths) modules
  _   <- traverse (P.appendLog "compile.log") out
  _   <- P.endProgress

  _       <- P.startProgress "Write modules" $ L.length deps
  modules <- P.async $ fmap P.concatModule deps
  _       <- P.outputCreatedModules modules
  _       <- P.endProgress

  -- HOOK
  maybeRunHook "post" (CliArguments.postHook args)


maybeRunHook :: String -> Maybe FilePath -> P.Pipeline ()
maybeRunHook _ Nothing  = return ()
maybeRunHook name (Just pathToScript) =
  P.startSpinner title
    >> P.hook pathToScript
    >>= P.appendLog (T.pack $ name ++ "-hook.log")
    >> P.endSpinner title
  where
    title = (T.pack $ name ++ " hook (" ++ pathToScript ++ ")" )
