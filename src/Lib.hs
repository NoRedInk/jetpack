module Lib
  ( run
  ) where

import CliArguments (readArguments)
import qualified CliArguments
import qualified Compile
import ConcatModule
import Config
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async.Lifted as Concurrent
import Control.Monad.State as State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.List.Utils as LU
import qualified Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Tree as Tree
import qualified DependencyTree
import qualified EntryPoints
import qualified Hooks
import qualified Init
import qualified Logger
import qualified Message
import qualified ProgressBar
import qualified ProgressSpinner
import qualified System.Console.AsciiProgress as AsciiProgress
import qualified System.Exit
import System.FilePath ((<.>), (</>))
import Task (Task, toTask)
import qualified Task
import qualified Version

run :: IO ()
run = do
  result <- AsciiProgress.displayConsoleRegions $ Task.runTask program
  case result of
    Right (Warnings warnings) -> Message.warning warnings
    Right (Info info) -> Message.info info
    Right (Success entrypoints) -> Message.success entrypoints
    Left err -> do
      _ <- Message.error err
      System.Exit.exitFailure

data Result
  = Success [T.Text]
  | Warnings T.Text
  | Info T.Text

program :: Task Result
program
  -- SETUP
 = do
  args <- readArguments
  if CliArguments.version args
    then return versionProgram
    else compileProgram args

compileProgram :: CliArguments.Args -> Task Result
compileProgram args
  -- SETUP
 = do
  _ <- Config.readConfig
  env <- State.get
  toolPaths <- Init.setup env
  _ <- traverse (Logger.clearLog env) Logger.allLogs
  -- HOOK
  maybeRunHook env Pre (CliArguments.preHook args)
  entryPoints <- EntryPoints.find env
  -- GETTING DEPENDENCY TREE
  _ <-
    ProgressBar.start
      (L.length entryPoints)
      "Finding dependencies for entrypoints"
  cache <- DependencyTree.readTreeCache env
  deps
    -- TODO Concurrent.forConcurrently $
     <-
    traverse (DependencyTree.build env cache) entryPoints
  _ <- DependencyTree.writeTreeCache env deps
  _ <- ProgressBar.end
  -- COMPILATION
  let modules = LU.uniq $ concatMap Tree.flatten deps
  _ <- ProgressBar.start (L.length modules) "Compiling"
  result <- traverse (Compile.compile env toolPaths) modules
  _ <- traverse (Logger.appendLog env Logger.compileLog . T.pack . show) result
  _ <-
    traverse
      (\Compile.Result {compiledFile, duration} ->
         Logger.appendLog env Logger.compileTime $
         (T.pack compiledFile) <> ": " <> (T.pack $ show duration) <> "\n")
      result
  _ <- ProgressBar.end
  _ <- ProgressBar.start (L.length deps) "Write modules"
  modules
    -- TODO Concurrent.forConcurrently $
     <-
    traverse (ConcatModule.wrap env) deps
  _ <- createdModulesJson env modules
  _ <- ProgressBar.end
  _ <-
    traverse
      (\Compile.Result {compiledFile, duration} ->
         printTime env compiledFile duration)
      result
  -- HOOK
  _ <- maybeRunHook env Post (CliArguments.postHook args)
  -- RETURN WARNINGS IF ANY
  let warnings = Data.Maybe.catMaybes (fmap Compile.warnings result)
  return $
    case warnings of
      [] -> Success $ fmap T.pack entryPoints
      xs -> Warnings $ T.unlines xs

versionProgram :: Result
versionProgram = Info Version.print

maybeRunHook :: Env -> Hook -> Maybe String -> Task ()
maybeRunHook env _ Nothing = return ()
maybeRunHook env type_ (Just hookScript) =
  ProgressSpinner.start title >> Hooks.run hookScript >>=
  Logger.appendLog env (log type_) >>
  ProgressSpinner.end title
  where
    title = (T.pack $ show type_ ++ " hook (" ++ hookScript ++ ")")
    log Pre = Logger.preHookLog
    log Post = Logger.postHookLog

data Hook
  = Pre
  | Post
  deriving (Show)

printTime :: Env -> FilePath -> Compile.Duration -> Task ()
printTime Env {args} path duration = do
  let Args {time} = args
  if time
    then do
      toTask $
        putStrLn $ T.unpack $ (T.pack path) <> ": " <> (T.pack $ show duration)
    else return ()

createdModulesJson :: Env -> [FilePath] -> Task ()
createdModulesJson Env {config} paths = do
  let encodedPaths = Aeson.encode paths
  let Config {temp_directory} = config
  let jsonPath = temp_directory </> "modules" <.> "json"
  _ <- toTask $ BL.writeFile jsonPath encodedPaths
  _ <- ProgressBar.step
  return ()
