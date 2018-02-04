module Lib
  ( run
  ) where

import CliArguments (Args(..), readArguments)
import qualified CliArguments
import qualified Compile
import ConcatModule
import Config
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async.Lifted as Concurrent
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
import ProgressBar (ProgressBar, complete, start, tick)
import qualified ProgressSpinner
import qualified System.Console.AsciiProgress as AsciiProgress
import qualified System.Exit
import System.FilePath ((<.>), (</>))
import Task (Task, toTask)
import qualified Task
import qualified Version

run :: IO ()
run = do
  result <- AsciiProgress.displayConsoleRegions $ Task.runExceptT program
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
  config <- Config.readConfig
  toolPaths <- Init.setup config
  _ <- traverse (Logger.clearLog config) Logger.allLogs
  -- HOOK
  maybeRunHook config Pre (CliArguments.preHook args)
  entryPoints <- EntryPoints.find args config
  -- GETTING DEPENDENCY TREE
  pg <- start (L.length entryPoints) "Finding dependencies for entrypoints"
  cache <- DependencyTree.readTreeCache config
  deps
    -- TODO Concurrent.forConcurrently $
     <-
    traverse (DependencyTree.build pg config cache) entryPoints
  _ <- DependencyTree.writeTreeCache config deps
  _ <- toTask $ complete pg
  -- COMPILATION
  let modules = LU.uniq $ concatMap Tree.flatten deps
  pg <- start (L.length modules) "Compiling"
  result <- traverse (Compile.compile pg args config toolPaths) modules
  _ <-
    traverse (Logger.appendLog config Logger.compileLog . T.pack . show) result
  _ <-
    traverse
      (\Compile.Result {compiledFile, duration} ->
         Logger.appendLog config Logger.compileTime $
         (T.pack compiledFile) <> ": " <> (T.pack $ show duration) <> "\n")
      result
  _ <- toTask $ complete pg
  pg <- start (L.length deps) "Write modules"
  modules
    -- TODO Concurrent.forConcurrently $
     <-
    traverse (ConcatModule.wrap pg config) deps
  _ <- toTask $ createdModulesJson pg config modules
  _ <- toTask $ complete pg
  _ <-
    traverse
      (\Compile.Result {compiledFile, duration} ->
         printTime args compiledFile duration)
      result
  -- HOOK
  _ <- maybeRunHook config Post (CliArguments.postHook args)
  -- RETURN WARNINGS IF ANY
  let warnings = Data.Maybe.catMaybes (fmap Compile.warnings result)
  return $
    case warnings of
      [] -> Success $ fmap T.pack entryPoints
      xs -> Warnings $ T.unlines xs

versionProgram :: Result
versionProgram = Info Version.print

maybeRunHook :: Config -> Hook -> Maybe String -> Task ()
maybeRunHook config _ Nothing = return ()
maybeRunHook config type_ (Just hookScript) = do
  spinner <- toTask $ ProgressSpinner.start title
  out <- Hooks.run hookScript
  Logger.appendLog config (log type_) out
  toTask $ ProgressSpinner.end spinner title
  where
    title = (T.pack $ show type_ ++ " hook (" ++ hookScript ++ ")")
    log Pre = Logger.preHookLog
    log Post = Logger.postHookLog

data Hook
  = Pre
  | Post
  deriving (Show)

printTime :: Args -> FilePath -> Compile.Duration -> Task ()
printTime args path duration = do
  let Args {time} = args
  if time
    then do
      toTask $
        putStrLn $ T.unpack $ (T.pack path) <> ": " <> (T.pack $ show duration)
    else return ()

createdModulesJson :: ProgressBar -> Config -> [FilePath] -> IO ()
createdModulesJson pg config paths = do
  let encodedPaths = Aeson.encode paths
  let Config {temp_directory} = config
  let jsonPath = temp_directory </> "modules" <.> "json"
  _ <- BL.writeFile jsonPath encodedPaths
  _ <- tick pg
  return ()
