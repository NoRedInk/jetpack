module Builder
  ( build
  ) where

import CliArguments (Args(..))
import qualified Compile
import ConcatModule
import Config
import qualified Control.Exception.Safe as ES
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.Utils as LU
import qualified Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Tree as Tree
import Dependencies (Dependency(..))
import qualified DependencyTree
import qualified EntryPoints
import qualified Hooks
import qualified Init
import qualified Logger
import qualified Message
import qualified Progress.Counter
import qualified Progress.Loader
import qualified Progress.Region
import qualified System.Console.Regions as CR
import qualified System.Exit
import System.FilePath ((<.>), (</>))

build :: Config.Config -> Args -> IO ()
build config args = do
  result <- CR.displayConsoleRegions $ ES.tryAny $ buildHelp config args
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

buildHelp :: Config.Config -> Args -> IO Result
buildHelp config args@Args {preHook, postHook} = do
  toolPaths <- Init.setup config
  _ <- traverse (Logger.clearLog config) Logger.allLogs
  -- HOOK
  loader <- Progress.Loader.startWithMsg "Pre hook:"
  maybeRunHook config Pre preHook
  _ <- Progress.Loader.stop loader
  entryPoints <- EntryPoints.find args config
  -- GETTING DEPENDENCY TREE
  let Config {temp_directory} = config
  deps <-
    Progress.Region.region
      "Building dependency tree for entrypoints:"
      (\region -> do
         cache <- DependencyTree.readTreeCache temp_directory
         deps <-
           Progress.Counter.mapConcurrently
             region
             (DependencyTree.build config cache)
             entryPoints
         DependencyTree.writeTreeCache temp_directory deps
         return deps)
  -- COMPILATION
  let modules = LU.uniq $ concatMap Tree.flatten deps
  result <-
    Progress.Region.region
      "Compiling:"
      (\region -> do
         result <-
           Progress.Counter.mapGroupConcurrently
             region
             (\Dependency {fileType} -> fileType)
             (Compile.compile args config toolPaths)
             modules
         _ <-
           traverse
             (Logger.appendLog config Logger.compileLog . T.pack . show)
             result
         _ <-
           traverse
             (\Compile.Result {compiledFile, duration} ->
                Logger.appendLog config Logger.compileTime $
                T.pack compiledFile <> ": " <> T.pack (show duration) <> "\n")
             result
         return result)
  _ <-
    Progress.Region.region
      "Write modules:"
      (\region -> do
         modules <-
           Progress.Counter.mapConcurrently
             region
             (ConcatModule.wrap config)
             deps
         _ <- createdModulesJson config modules
         return modules)
  _ <- traverse (Compile.printTime args) result
  -- HOOK
  loader <- Progress.Loader.startWithMsg "Post hook:"
  maybeRunHook config Post postHook
  _ <- Progress.Loader.stop loader
  -- RETURN WARNINGS IF ANY
  let warnings = Data.Maybe.catMaybes (fmap Compile.warnings result)
  case warnings of
    [] -> return $ Success entryPoints
    xs -> return $ Warnings entryPoints xs

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

createdModulesJson :: Config -> [FilePath] -> IO ()
createdModulesJson config paths = do
  let encodedPaths = Aeson.encode paths
  let Config {temp_directory} = config
  let jsonPath = temp_directory </> "modules" <.> "json"
  _ <- BL.writeFile jsonPath encodedPaths
  -- _ <- tick pg
  return ()
