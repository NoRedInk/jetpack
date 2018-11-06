module Builder
  ( build
  ) where

import CliArguments (Args(..), Minify(..))
import qualified Compile
import ConcatModule
import Config
import qualified Control.Concurrent.Async.Lifted as Concurrent
import qualified Control.Exception.Safe as ES
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.List.Utils as LU
import qualified Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Tree as Tree
import qualified DependencyTree
import qualified EntryPoints
import GHC.IO.Handle
import qualified Init
import qualified Logger
import qualified Message
import ProgressBar (ProgressBar, complete, start, tick)
import qualified System.Console.AsciiProgress as AsciiProgress
import System.Exit
import System.FilePath ((<.>), (</>))
import System.Process

build :: Config.Config -> Args -> IO ()
build config args = do
  result <-
    AsciiProgress.displayConsoleRegions $ ES.tryAny $ buildHelp config args
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
buildHelp config args = do
  toolPaths <- Init.setup config
  _ <- traverse (Logger.clearLog config) Logger.allLogs
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
  _minified <- Concurrent.mapConcurrently (minfiyModule pg config args) modules
  -- RETURN WARNINGS IF ANY
  let warnings = Data.Maybe.catMaybes (fmap Compile.warnings result)
  case warnings of
    [] -> return $ Success entryPoints
    xs -> return $ Warnings entryPoints xs

minfiyModule :: ProgressBar -> Config -> Args -> FilePath -> IO ()
minfiyModule _pg Config {minifierPath} Args {minify} output = do
  case minify of
    DontMinify -> return ()
    Minify -> do
      (_, Just out, Just err, ph) <-
        createProcess
          (proc "bash" ["-c", minifierPath <> " " <> output])
          {std_out = CreatePipe, std_err = CreatePipe}
      _ec <- waitForProcess ph
      content <- hGetContents out
      print content
      _errContent <- hGetContents err
      pure ()

createdModulesJson :: ProgressBar -> Config -> [FilePath] -> IO ()
createdModulesJson pg config paths = do
  let encodedPaths = Aeson.encode paths
  let Config {tempDir} = config
  let jsonPath = tempDir </> "modules" <.> "json"
  _ <- BL.writeFile jsonPath encodedPaths
  _ <- tick pg
  return ()
