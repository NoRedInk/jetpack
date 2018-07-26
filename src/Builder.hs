module Builder
  ( build
  ) where

import CliArguments (Args(..))
import qualified Compile
import ConcatModule
import Config
import qualified Control.Concurrent.Async.Lifted as Concurrent
import qualified Control.Exception.Safe as ES
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.List.Utils as LU
import qualified Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Tree as Tree
import Dependencies
import qualified DependencyTree
import qualified EntryPoints
import qualified Hooks
import qualified Init
import qualified Logger
import qualified Message
import qualified Parser.Ast as Ast
import ProgressBar (ProgressBar, complete, start, tick)
import qualified System.Console.AsciiProgress as AsciiProgress
import qualified System.Exit
import System.FilePath ((<.>), (</>))
import System.Process
import qualified Utils.Tree
import qualified WatchMode

build :: Config.Config -> Args -> WatchMode.Mode -> IO ()
build config args mode = do
  result <-
    AsciiProgress.displayConsoleRegions $ ES.tryAny $ buildHelp config args mode
  printResult result

data Result
  = Success [FilePath]
  | Warnings [FilePath]
             [T.Text]

printResult :: Either ES.SomeException Result -> IO ()
printResult result =
  case result of
    Right (Warnings entryPoints warnings) -> do
      _ <- traverse (TIO.putStrLn) warnings
      _ <- Message.list $ T.pack <$> entryPoints
      Message.warning "Succeeded with Warnings!"
    Right (Success entryPoints) -> do
      _ <- Message.list $ T.pack <$> entryPoints
      Message.success $ T.pack "Succeeded"
    Left err -> do
      _ <- putStrLn $ show err
      _ <- Message.error $ T.pack "Failed!"
      System.Exit.exitFailure

buildHelp :: Config.Config -> Args -> WatchMode.Mode -> IO Result
buildHelp config args@Args {preHook, postHook} mode = do
  toolPaths <- Init.setup config
  _ <- traverse (Logger.clearLog config) Logger.allLogs
  -- HOOK
  maybeRunHook config Pre preHook
  entryPoints <- EntryPoints.find args config
  -- GETTING DEPENDENCY TREE
  pg <- start (L.length entryPoints) "Finding dependencies for entrypoints"
  let Config {tempDir} = config
  cache <- DependencyTree.readTreeCache tempDir
  deps <-
    Concurrent.mapConcurrently
      (DependencyTree.build pg config cache)
      entryPoints
  DependencyTree.writeTreeCache tempDir deps
  complete pg
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
  complete pg
  pg <- start (L.length deps) "Write modules"
  modules <-
    Concurrent.mapConcurrently
      (\dep -> fmap ((,) dep) (ConcatModule.wrap pg config dep))
      deps
  _ <-
    do createdModulesJson pg config (fmap snd modules)
       complete pg
       traverse (Compile.printTime args) result
  maybeRunReplay config mode modules
  -- HOOK
  maybeRunHook config Post postHook
  -- RETURN WARNINGS IF ANY
  let warnings = Data.Maybe.catMaybes (fmap Compile.warnings result)
  case warnings of
    [] -> return $ Success entryPoints
    xs -> return $ Warnings entryPoints xs

maybeRunReplay ::
     Config -> WatchMode.Mode -> [(DependencyTree, FilePath)] -> IO ()
maybeRunReplay Config {replayScriptPath, sourceDir} mode modules =
  case mode of
    WatchMode.NoHacks -> pure ()
    WatchMode.Replay -> traverse_ (runReplay sourceDir replayScriptPath) modules

runReplay :: FilePath -> Maybe FilePath -> (DependencyTree, FilePath) -> IO ()
runReplay _ Nothing _ = pure ()
runReplay _sourceDir (Just replayScriptPath) (deps, path) = do
  let elmEntry = Tree.rootLabel <$> Utils.Tree.searchNode isElmEntry deps
  case elmEntry of
    Nothing -> pure ()
    Just _ -> do
      let moduleName = "_NoRedInk\\$noredink\\$Page_Login_Main\\$"
      _ <-
        createProcess
          (proc
             "bash"
             ["-c", replayScriptPath <> " " <> path <> " " <> moduleName])
      pure ()

isElmEntry :: DependencyTree -> Bool
isElmEntry tree = (fileType $ Tree.rootLabel tree) == Ast.Elm

maybeRunHook :: Config -> Hook -> Maybe String -> IO ()
maybeRunHook _ _ Nothing = return ()
maybeRunHook config type_ (Just hookScript) = do
  out <- Hooks.run hookScript
  Logger.appendLog config (log type_) out
    -- TODO title = T.pack $ show type_ ++ " hook (" ++ hookScript ++ ")"
  where
    log Pre = Logger.preHookLog
    log Post = Logger.postHookLog

data Hook
  = Pre
  | Post
  deriving (Show)

createdModulesJson :: ProgressBar -> Config -> [FilePath] -> IO ()
createdModulesJson pg config paths = do
  let encodedPaths = Aeson.encode paths
  let Config {tempDir} = config
  let jsonPath = tempDir </> "modules" <.> "json"
  _ <- BL.writeFile jsonPath encodedPaths
  _ <- tick pg
  return ()
