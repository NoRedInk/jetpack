module Builder
  ( build
  ) where

import CliArguments (Args(..))
import qualified Compile
import ConcatModule
import Config (Config(Config))
import qualified Config
import qualified Control.Concurrent.Async.Lifted as Concurrent
import qualified Control.Exception.Safe as ES
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.List.Utils as LU
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Tree as Tree
import qualified DependencyTree
import qualified EntryPoints
import qualified Init
import qualified Logger
import qualified Message
import ProgressBar (ProgressBar, complete, start, tick)
import qualified System.Console.AsciiProgress as AsciiProgress
import qualified System.Directory as Dir
import qualified System.Exit
import System.FilePath ((<.>), (</>))
import qualified System.FilePath as FP
import qualified System.FilePath.Glob as Glob

build :: Config -> Args -> IO ()
build config args = do
  result <-
    AsciiProgress.displayConsoleRegions $ ES.tryAny $ buildHelp config args
  printResult result

printResult :: Either ES.SomeException [FilePath] -> IO ()
printResult result =
  case result of
    Right entryPoints -> do
      _ <- Message.list $ T.pack <$> entryPoints
      Message.success $ T.pack "Succeeded"
    Left err -> do
      _ <- putStrLn $ show err
      _ <- Message.error $ T.pack "Failed!"
      System.Exit.exitFailure

buildHelp :: Config -> Args -> IO [FilePath]
buildHelp config@Config {Config.tempDir, Config.logDir} args = do
  toolPaths <- Init.setup config
  traverse_ (Logger.clearLog logDir) Logger.allLogs
  checkElmStuffConsistency config
  entryPoints <- EntryPoints.find args config
  -- GETTING DEPENDENCY TREE
  pg <- start (L.length entryPoints) "Finding dependencies for entrypoints"
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
    traverse (Logger.appendLog logDir Logger.compileLog . T.pack . show) result
  _ <-
    traverse
      (\Compile.Result {compiledFile, duration} ->
         Logger.appendLog logDir Logger.compileTime $
         T.pack compiledFile <> ": " <> T.pack (show duration) <> "\n")
      result
  complete pg
  pg <- start (L.length deps) "Write modules"
  modules <- Concurrent.mapConcurrently (ConcatModule.wrap pg config) deps
  _ <-
    do createdModulesJson pg tempDir modules
       complete pg
       traverse (Compile.printTime args) result
  -- RETURN WARNINGS IF ANY
  return entryPoints

checkElmStuffConsistency :: Config.Config -> IO ()
checkElmStuffConsistency Config.Config {elmRoot, logDir} = do
  files <-
    mconcat .
    filter ((/=) 2 . length) . L.groupBy sameModule . L.sortBy sortModules <$>
    Glob.glob (Config.unElmRoot elmRoot </> "elm-stuff/0.19.0/*.elm[io]")
  Logger.appendLog logDir Logger.consistencyLog . mconcat $
    L.intersperse "\n" $ fmap T.pack files
  traverse_ Dir.removeFile files

sameModule :: FilePath -> FilePath -> Bool
sameModule a b = FP.dropExtension a == FP.dropExtension b

sortModules :: FilePath -> FilePath -> Ordering
sortModules a b = compare (FP.dropExtension a) (FP.dropExtension b)

createdModulesJson :: ProgressBar -> Config.TempDir -> [FilePath] -> IO ()
createdModulesJson pg tempDir paths = do
  let encodedPaths = Aeson.encode paths
  let jsonPath = Config.unTempDir tempDir </> "modules" <.> "json"
  _ <- BL.writeFile jsonPath encodedPaths
  _ <- tick pg
  return ()
