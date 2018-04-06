module Builder
  ( build
  ) where

import CliArguments (Args(..))
import qualified Compile
import ConcatModule
import Config
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
import Task (Task, lift)
import qualified Task

build :: Config.Config -> Args -> IO ()
build config args = do
  result <-
    AsciiProgress.displayConsoleRegions $
    Task.runExceptT (buildHelp config args)
  case result of
    Right (Warnings entryPoints warnings) -> do
      _ <- Message.warning warnings
      Message.whichEntryPoints entryPoints
    Right (Success entryPoints) -> do
      _ <- Message.success
      Message.whichEntryPoints entryPoints
    Left err -> do
      _ <- Message.error err
      System.Exit.exitFailure

data Result
  = Success [FilePath]
  | Warnings [FilePath]
             [T.Text]

buildHelp :: Config.Config -> Args -> Task Result
buildHelp config args@Args {preHook, postHook} = do
  toolPaths <- Init.setup config
  _ <- traverse (Logger.clearLog config) Logger.allLogs
  -- HOOK
  maybeRunHook config Pre preHook
  entryPoints <- EntryPoints.find args config
  -- GETTING DEPENDENCY TREE
  pg <- start (L.length entryPoints) "Finding dependencies for entrypoints"
  let Config {temp_directory} = config
  cache <- lift $ DependencyTree.readTreeCache temp_directory
  deps <-
    Concurrent.mapConcurrently
      (DependencyTree.build pg config cache)
      entryPoints
  lift $ DependencyTree.writeTreeCache temp_directory deps
  lift $ complete pg
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
         T.pack compiledFile <> ": " <> T.pack (show duration) <> "\n")
      result
  lift $ complete pg
  pg <- start (L.length deps) "Write modules"
  modules <- Concurrent.mapConcurrently (ConcatModule.wrap pg config) deps
  _ <-
    lift $ do
      createdModulesJson pg config modules
      complete pg
      traverse (Compile.printTime args) result
  -- HOOK
  maybeRunHook config Post postHook
  -- RETURN WARNINGS IF ANY
  let warnings = Data.Maybe.catMaybes (fmap Compile.warnings result)
  case warnings of
    [] -> return $ Success entryPoints
    xs -> return $ Warnings entryPoints xs

maybeRunHook :: Config -> Hook -> Maybe String -> Task ()
maybeRunHook _ _ Nothing = return ()
maybeRunHook config type_ (Just hookScript) = do
  spinner <- lift $ ProgressSpinner.start title
  out <- Hooks.run hookScript
  Logger.appendLog config (log type_) out
  lift $ ProgressSpinner.end spinner title
  where
    title = T.pack $ show type_ ++ " hook (" ++ hookScript ++ ")"
    log Pre = Logger.preHookLog
    log Post = Logger.postHookLog

data Hook
  = Pre
  | Post
  deriving (Show)

createdModulesJson :: ProgressBar -> Config -> [FilePath] -> IO ()
createdModulesJson pg config paths = do
  let encodedPaths = Aeson.encode paths
  let Config {temp_directory} = config
  let jsonPath = temp_directory </> "modules" <.> "json"
  _ <- BL.writeFile jsonPath encodedPaths
  _ <- tick pg
  return ()
