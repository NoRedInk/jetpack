{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
-}
module Compile where

import Config (Config (..))
-- import Control.Concurrent.Async.Lifted as Async (forConcurrently)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Data.List as L
import Data.Text as T
import Data.Time.Clock
import Dependencies (Dependency (..))
import GHC.IO.Handle
import Parser.Ast as Ast
import System.Directory (copyFile)
import System.Exit
import System.FilePath ((</>))
import System.Process
import Task (Task)
import ToolPaths
import Utils.Files (pathToFileName)

newtype Compiler = Compiler { runCompiler :: FilePath -> FilePath -> Task [T.Text] }

compileModules :: Config -> ToolPaths ->  [Dependency] -> Task ()
compileModules config@Config {log_directory} toolPaths modules = do
  output <- traverse (compile config toolPaths) modules
  let log = T.unlines $ fmap T.unlines output
  lift $ writeFile (log_directory </> "compile.log") $ T.unpack log
  return ()

compile :: Config -> ToolPaths ->  Dependency -> Task [T.Text]
compile config toolPaths Dependency {fileType, filePath} = do
  let (c, outputType) = compiler fileType config toolPaths
  let outputPath = buildArtifactPath config outputType filePath
  (runCompiler c) filePath outputPath

compiler :: Ast.SourceType -> Config -> ToolPaths -> (Compiler, String)
compiler fileType config ToolPaths{elmMake, sassc, coffee} =
  case fileType of
    Ast.Elm    -> (elmCompiler config elmMake, "js")
    Ast.Js     -> (jsCompiler, "js")
    Ast.Coffee -> (coffeeCompiler coffee, "js")
    Ast.Sass   -> (sassCompiler config sassc, "css")

buildArtifactPath :: Config -> String -> FilePath -> String
buildArtifactPath Config{temp_directory} extension inputPath =
  temp_directory </> pathToFileName inputPath extension

---------------
-- COMPILERS --
---------------

elmCompiler :: Config -> FilePath -> Compiler
elmCompiler Config{elm_root_directory} elmMake = Compiler $ \input output -> do
  -- TODO use elm_root_directory instead of the "../" below
  -- also pass in absolute paths
  let cmd = elmMake ++ " " ++ "../" ++ input ++ " --output " ++ "../" ++ output
  runCmd cmd $ Just elm_root_directory

coffeeCompiler :: FilePath -> Compiler
coffeeCompiler coffee = Compiler $ \input output -> do
  let cmd = coffee ++ " -p " ++ input ++ " > " ++ output
  runCmd cmd Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: Compiler
jsCompiler = Compiler $ \input output -> lift $ do
  copyFile input output
  currentTime <- getCurrentTime
  let commandFinished = T.pack $ show currentTime
  return [commandFinished, T.concat ["moved ", T.pack input, " => ", T.pack output]]

sassCompiler :: Config -> FilePath -> Compiler
sassCompiler Config {sass_load_paths} sassc = Compiler $ \input output -> do
  let loadPath = L.intercalate ":" sass_load_paths
  let cmd = "SASS_PATH=" ++ loadPath ++ " " ++ sassc ++ " " ++ input ++ " " ++ output
  runCmd cmd Nothing

runCmd :: String -> Maybe String -> Task [T.Text]
runCmd cmd maybeCwd = do
  -- TODO: handle exit status here
  (_, Just out, _, ph) <- lift $ createProcess (proc "bash" ["-c", cmd])
    { std_out = CreatePipe
    , cwd = maybeCwd
    }
  ec <- lift $ waitForProcess ph
  case ec of
      ExitSuccess   -> lift $ do
        content <- hGetContents out
        currentTime <- getCurrentTime
        let commandFinished = T.pack $ show currentTime
        return [commandFinished, T.pack cmd, T.pack content]
      ExitFailure _ -> throwError []
