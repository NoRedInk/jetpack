{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import qualified CliArguments
import Control.Concurrent (threadDelay)
import qualified Control.Monad.Free as Free
import qualified Data.List as L
import qualified Data.List.Utils as LU
import qualified Data.Maybe
import qualified Data.Text as T
import qualified Data.Tree as Tree
import qualified Interpreter.Pipeline as PipelineI
import qualified Logger
import qualified Message
import qualified Pipeline as P
import qualified System.Console.AsciiProgress as AsciiProgress
import qualified System.Exit
import qualified Task

run :: IO ()
run = do
  result <-
    AsciiProgress.displayConsoleRegions $
    Task.runTask $ Free.foldFree PipelineI.interpreter program
  case result of
    Right (Warnings warnings) -> Message.warning warnings
    Right (Info info) -> Message.info info
    Right Success -> Message.success
    Left err -> do
      _ <- Message.error err
      System.Exit.exitFailure

data Result
  = Success
  | Warnings T.Text
  | Info T.Text

program :: P.Pipeline Result
program
  -- SETUP
 = do
  args <- P.readCliArgs
  if CliArguments.version args
    then versionProgram
    else compileProgram args

compileProgram :: CliArguments.Args -> P.Pipeline Result
compileProgram args
  -- SETUP
 = do
  _ <- P.readConfig (CliArguments.configPath args)
  toolPaths <- P.setup
  _ <- traverse P.clearLog Logger.allLogs
  -- HOOK
  maybeRunHook Pre (CliArguments.preHook args)
  entryPoints <- P.findEntryPoints
  -- GETTING DEPENDENCY TREE
  _ <-
    P.startProgress "Finding dependencies for entrypoints" $
    L.length entryPoints
  cache <- P.readDependencyCache
  deps <- P.async $ fmap (P.findDependency cache) entryPoints
  _ <- P.writeDependencyCache deps
  _ <- P.endProgress
  -- COMPILATION
  let modules = LU.uniq $ concatMap Tree.flatten deps
  _ <- P.startProgress "Compiling" $ L.length modules
  out <- traverse (P.compile toolPaths) modules
  _ <- traverse (\(log, _) -> P.appendLog "compile.log" log) out
  _ <- P.endProgress
  _ <- P.startProgress "Write modules" $ L.length deps
  modules <- P.async $ fmap P.concatModule deps
  _ <- P.outputCreatedModules modules
  _ <- P.endProgress
  -- HOOK
  _ <- maybeRunHook Post (CliArguments.postHook args)
  -- RETURN WARNINGS IF ANY
  let warnings = Data.Maybe.catMaybes (fmap snd out)
  return $
    case warnings of
      [] -> Success
      xs -> Warnings $ T.unlines xs

versionProgram :: P.Pipeline Result
versionProgram = do
  version <- P.version
  return $ Info version

maybeRunHook :: Hook -> Maybe String -> P.Pipeline ()
maybeRunHook _ Nothing = return ()
maybeRunHook type_ (Just hookScript) =
  P.startSpinner title >> P.hook hookScript >>= P.appendLog (log type_) >>
  P.endSpinner title
  where
    title = (T.pack $ show type_ ++ " hook (" ++ hookScript ++ ")")
    log Pre = Logger.preHookLog
    log Post = Logger.postHookLog

data Hook
  = Pre
  | Post
  deriving (Show)
