module Builder
  ( build
  ) where

import CliArguments (Args(..))
import qualified Compile
import ConcatModule
import Config
import qualified Control.Concurrent.Async.Lifted as Concurrent
import qualified Control.Exception.Safe as ES
import Control.Monad (when)
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
import System.FilePath
       ((<.>), (</>), replaceExtension, takeExtension)
import qualified System.FilePath.Glob as Glob

build :: Config.Config -> Args -> IO ()
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

checkElmArtifact :: FilePath -> IO ()
checkElmArtifact filePath = do
  case takeExtension filePath of
    ".elmi" -> checkFile filePath ".elmo"
    ".elmo" -> checkFile filePath ".elmi"
    _ -> return ()
  where
    checkFile path ext = do
      fileExists <- Dir.doesFileExist $ replaceExtension path ext
      when (not fileExists) $ Dir.removeFile path

checkElmStuffConsistency :: Config.Config -> IO ()
checkElmStuffConsistency Config.Config {elmRoot} = do
  files <- Glob.glob $ elmRoot </> "elm-stuff/0.19.0/*.elm[io]"
  traverse_ checkElmArtifact files

buildHelp :: Config.Config -> Args -> IO [FilePath]
buildHelp config args = do
  toolPaths <- Init.setup config
  traverse_ (Logger.clearLog config) Logger.allLogs
  checkElmStuffConsistency config
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
  modules <- Concurrent.mapConcurrently (ConcatModule.wrap pg config) deps
  _ <-
    do createdModulesJson pg config modules
       complete pg
       traverse (Compile.printTime args) result
  -- RETURN WARNINGS IF ANY
  return entryPoints

createdModulesJson :: ProgressBar -> Config -> [FilePath] -> IO ()
createdModulesJson pg config paths = do
  let encodedPaths = Aeson.encode paths
  let Config {tempDir} = config
  let jsonPath = tempDir </> "modules" <.> "json"
  _ <- BL.writeFile jsonPath encodedPaths
  _ <- tick pg
  return ()
