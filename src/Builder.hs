module Builder
  ( build
  ) where

import CliArguments (Args(..))
import qualified Compile
import ConcatModule
import Config (Config(Config))
import qualified Config
import qualified Control.Concurrent
import qualified Control.Concurrent.Async.Lifted as Concurrent
import qualified Control.Exception.Safe as ES
import Control.Lens.Indexed as Indexed hiding ((<.>))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (traverse_)
import qualified Data.List as L
import qualified Data.List.Utils as LU
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Tree as Tree
import qualified Dependencies
import qualified DependencyTree
import qualified EntryPoints
import qualified Init
import qualified Logger
import qualified Message
import qualified Parser.Ast as Ast
import qualified System.Console.Regions as CR
import qualified System.Directory as Dir
import qualified System.Exit
import System.FilePath ((<.>), (</>))
import qualified System.FilePath as FP
import qualified System.FilePath.Glob as Glob
import qualified ToolPaths

build :: Config -> Args -> IO ()
build config args = do
  result <- ES.tryAny $ buildHelp config args
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

buildHelp :: Config.Config -> Args -> IO [FilePath]
buildHelp config@Config { Config.tempDir
                        , Config.logDir
                        , Config.elmRoot
                        , Config.outputDir
                        , Config.elmPath
                        , Config.coffeePath
                        , Config.entryPoints
                        } args =
  CR.displayConsoleRegions $ do
    toolPaths <- Init.setup tempDir logDir outputDir elmPath coffeePath
    traverse_ (Logger.clearLog logDir) Logger.allLogs
    checkElmStuffConsistency logDir elmRoot
    entryPoints <- EntryPoints.find args entryPoints
  -- GETTING DEPENDENCY TREE
    deps <-
      withSpinner $ \subRegion endSpinner -> do
        _ <-
          CR.setConsoleRegion subRegion $
          T.pack " Finding dependencies for entrypoints."
        cache <- DependencyTree.readTreeCache tempDir
        deps <-
          Concurrent.mapConcurrently
            (DependencyTree.build config cache)
            entryPoints
        DependencyTree.writeTreeCache tempDir deps
        endSpinner "All dependencies found."
        pure deps
  -- COMPILATION
    let modules = LU.uniq $ concatMap Tree.flatten deps
    let Compile.Groupped {elm, js, coffee} = Compile.group modules
    result <-
      mconcat <$>
      Concurrent.mapConcurrently
        (compile args config toolPaths)
        [(Ast.Elm, elm), (Ast.Js, js), (Ast.Coffee, coffee)]
    withSpinner $ \subRegion endSpinner -> do
      CR.setConsoleRegion subRegion $ T.pack "Writing modules."
      modules <- Concurrent.mapConcurrently (ConcatModule.wrap config) deps
      createdModulesJson tempDir modules
      endSpinner "Modules written."
      traverse_ (Compile.printTime args) result
  -- RETURN WARNINGS IF ANY
    return entryPoints

compile ::
     (Show a, TraversableWithIndex a t)
  => Args
  -> Config
  -> ToolPaths.ToolPaths
  -> (Ast.SourceType, t Dependencies.Dependency)
  -> IO (t Compile.Result)
compile args config@Config {Config.logDir} toolPaths (sourceType, modules) =
  withSpinner $ \subRegion endSpinner -> do
    _ <-
      CR.setConsoleRegion subRegion $
      " " <> show sourceType <> " (0/" <> show (length modules) <> ") "
    CR.withConsoleRegion (CR.InLine subRegion) $ \region -> do
      result <-
        Indexed.itraverse
          (\index m -> do
             r <- Compile.compile region args config toolPaths m
             CR.setConsoleRegion subRegion $
               " " <> show sourceType <> " " <> show index <> "/" <>
               show (length modules) <>
               ") "
             pure r)
          modules
      _ <-
        traverse
          (Logger.appendLog logDir Logger.compileLog . T.pack . show)
          result
      _ <-
        traverse
          (\Compile.Result {compiledFile, duration} ->
             Logger.appendLog logDir Logger.compileTime $
             T.pack compiledFile <> ": " <> T.pack (show duration) <> "\n")
          result
      endSpinner $
        T.pack $ "Compilation of " <> show sourceType <> " successful."
      pure result

withSpinner :: (CR.ConsoleRegion -> (T.Text -> IO ()) -> IO a) -> IO a
withSpinner go =
  let spin spinnerRegion counter = do
        Control.Concurrent.threadDelay 100000
        CR.setConsoleRegion spinnerRegion $ symbol counter
        spin spinnerRegion ((counter + 1) `mod` 8)
  in CR.withConsoleRegion CR.Linear $ \parentRegion -> do
       CR.withConsoleRegion (CR.InLine parentRegion) $ \spinnerRegion -> do
         CR.appendConsoleRegion spinnerRegion $ T.pack "\\"
         threadId <- Control.Concurrent.forkIO $ spin spinnerRegion 0
         result <-
           CR.withConsoleRegion (CR.InLine parentRegion) $ \subRegion -> do
             go subRegion $ \message -> do
               Control.Concurrent.killThread threadId
               CR.finishConsoleRegion parentRegion (message <> " ✔")
         pure result

symbol :: Int -> T.Text
symbol 0 = "⣾"
symbol 1 = "⣽"
symbol 2 = "⣻"
symbol 3 = "⢿"
symbol 4 = "⡿"
symbol 5 = "⣟"
symbol 6 = "⣯"
symbol _ = "⣷"

checkElmStuffConsistency :: Config.LogDir -> Config.ElmRoot -> IO ()
checkElmStuffConsistency logDir elmRoot = do
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

createdModulesJson :: Config.TempDir -> [FilePath] -> IO ()
createdModulesJson tempDir paths = do
  let encodedPaths = Aeson.encode paths
  let jsonPath = Config.unTempDir tempDir </> "modules" <.> "json"
  _ <- BL.writeFile jsonPath encodedPaths
  return ()
