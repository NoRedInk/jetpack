{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
-}
module Compile where

import Config (Config(..))
import Control.Monad.Except (throwError)
import Data.List as L
import Data.Text as T
import Data.Time.Clock
import Dependencies (Dependency(..))
import Env
import Error
import GHC.IO.Handle
import Parser.Ast as Ast
import qualified ProgressBar
import System.Directory (copyFile)
import System.Exit
import System.FilePath ((</>))
import System.Process
import Task (Task, getArgs, getConfig, toTask)
import ToolPaths
import Utils.Files (pathToFileName)

newtype Compiler = Compiler
  { runCompiler :: FilePath -> FilePath -> Task (T.Text, Maybe T.Text)
  }

compile :: ToolPaths -> Dependency -> Task (T.Text, Maybe T.Text)
compile toolPaths Dependency {fileType, filePath} = do
  config <- Task.getConfig
  let (c, outputType) = compiler fileType config toolPaths
  let outputPath = buildArtifactPath config outputType filePath
  (log, warnings) <- (runCompiler c) filePath outputPath
  return (log, warnings)

compiler :: Ast.SourceType -> Config -> ToolPaths -> (Compiler, String)
compiler fileType config ToolPaths {elmMake, sassc, coffee} =
  case fileType of
    Ast.Elm -> (elmCompiler config elmMake, "js")
    Ast.Js -> (jsCompiler, "js")
    Ast.Coffee -> (coffeeCompiler coffee, "js")
    Ast.Sass -> (sassCompiler config sassc, "css")

buildArtifactPath :: Config -> String -> FilePath -> String
buildArtifactPath Config {temp_directory} extension inputPath =
  temp_directory </> pathToFileName inputPath extension

---------------
-- COMPILERS --
---------------
elmCompiler :: Config -> FilePath -> Compiler
elmCompiler Config {elm_root_directory} elmMake =
  Compiler $ \input output
  -- TODO use elm_root_directory instead of the "../" below
  -- also pass in absolute paths
   -> do
    Args {debug, warn} <- Task.getArgs
    let debugFlag =
          if debug
            then " --debug"
            else ""
    let warnFlag =
          if warn
            then " --warn"
            else ""
    let cmd =
          elmMake ++
          " " ++
          "../" ++
          input ++
          " --output " ++ "../" ++ output ++ debugFlag ++ " --yes" ++ warnFlag
    runCmd cmd $ Just elm_root_directory

coffeeCompiler :: FilePath -> Compiler
coffeeCompiler coffee =
  Compiler $ \input output -> do
    let cmd = coffee ++ " -p " ++ input ++ " > " ++ output
    runCmd cmd Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: Compiler
jsCompiler =
  Compiler $ \input output -> do
    _ <- toTask $ copyFile input output
    _ <- ProgressBar.step
    currentTime <- toTask getCurrentTime
    let commandFinished = T.pack $ show currentTime
    return
      ( T.unlines
          [ commandFinished
          , T.concat ["moved ", T.pack input, " => ", T.pack output]
          ]
      , Nothing)

sassCompiler :: Config -> FilePath -> Compiler
sassCompiler Config {sass_load_paths} sassc =
  Compiler $ \input output -> do
    let loadPath = L.intercalate ":" sass_load_paths
    let cmd =
          "SASS_PATH=" ++
          loadPath ++ " " ++ sassc ++ " " ++ input ++ " " ++ output
    runCmd cmd Nothing

runCmd :: String -> Maybe String -> Task (T.Text, Maybe T.Text)
runCmd cmd maybeCwd = do
  (_, Just out, Just err, ph) <-
    toTask $
    createProcess
      (proc "bash" ["-c", cmd])
      {std_out = CreatePipe, std_err = CreatePipe, cwd = maybeCwd}
  ec <- toTask $ waitForProcess ph
  Args {warn} <- Task.getArgs
  case ec of
    ExitSuccess -> do
      content <- toTask $ hGetContents out
      errContent <- toTask $ hGetContents err
      _ <- ProgressBar.step
      currentTime <- toTask $ getCurrentTime
      let commandFinished = T.pack $ show currentTime
      let warnText =
            T.pack <$>
            if warn && errContent /= ""
              then Just errContent
              else Nothing
      return (T.unlines [commandFinished, T.pack cmd, T.pack content], warnText)
    ExitFailure _ -> do
      content <- toTask $ hGetContents out
      errContent <- toTask $ hGetContents err
      throwError [CompileError cmd (content ++ errContent)]
