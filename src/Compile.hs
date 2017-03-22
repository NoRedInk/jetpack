{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
-}
module Compile where

import Config (Config (..))
import Control.Concurrent.Async.Lifted as Async
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.List as L
import Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Dependencies (Dependency (..))
import GHC.IO.Handle
import Parser.Ast as Ast
import System.Console.AsciiProgress
import System.Directory (copyFile)
import System.Exit
import System.FilePath ((</>))
import System.Posix.Files
import System.Process
import Task (Task)
import Utils.Files (pathToFileName)

newtype Compiler = Compiler { runCompiler :: FilePath -> FilePath -> Task [T.Text] }

compileModules :: Config -> [Dependency] -> Task ()
compileModules config@Config {log_directory} modules = do
  e <- lift $ displayConsoleRegions $ do
    pg <- newProgressBar def
      { pgTotal = toInteger $ L.length modules
      , pgOnCompletion = Just "Compiled :percent after :elapsed seconds"
      , pgCompletedChar = '█'
      , pgPendingChar = '░'
      , pgFormat = "Compiling ╢:bar╟ :current/:total"
      }
    e <- runExceptT
      $ Async.forConcurrently modules
      $ compile pg config
    complete pg
    return e
  case e of
    Left err -> throwError err
    Right output  -> do
      let log = T.unlines $ fmap T.unlines output
      lift $ writeFile (log_directory </> "compile.log") $ T.unpack log
      return ()

compile :: ProgressBar -> Config -> Dependency -> Task [T.Text]
compile pg config Dependency {fileType, filePath, lastModificationTime} = do
  let (c, outputType) = compiler fileType config pg
  let outputPath = buildArtifactPath config outputType filePath
  status <- lift $ getFileStatus filePath
  let fileChanged = lastModificationTime /= Just (posixSecondsToUTCTime $ modificationTimeHiRes status)
  if fileChanged || fileType == Ast.Elm then
    (runCompiler c) filePath outputPath
  else
    return []

compiler :: Ast.SourceType -> Config -> ProgressBar -> (Compiler, String)
compiler fileType config pg =
  case fileType of
    Ast.Elm    -> (elmCompiler pg config, "js")
    Ast.Js     -> (jsCompiler pg, "js")
    Ast.Coffee -> (coffeeCompiler pg, "js")
    Ast.Sass   -> (sassCompiler pg config, "css")

buildArtifactPath :: Config -> String -> FilePath -> String
buildArtifactPath Config{temp_directory} extension inputPath =
  temp_directory </> pathToFileName inputPath extension

---------------
-- COMPILERS --
---------------

elmCompiler :: ProgressBar -> Config -> Compiler
elmCompiler pg Config{elm_root_directory} = Compiler $ \input output -> do
  -- TODO use elm_root_directory instead of the "../" below
  -- also pass in absolute paths
  let elmMake = "elm-make " ++ "../" ++ input ++ " --output " ++ "../" ++ output
  runCmd pg elmMake $ Just elm_root_directory

coffeeCompiler :: ProgressBar -> Compiler
coffeeCompiler pg = Compiler $ \input output -> do
  let coffee = "coffee -p " ++ input ++ " > " ++ output
  runCmd pg coffee Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: ProgressBar -> Compiler
jsCompiler pg = Compiler $ \input output -> lift $ do
  copyFile input output
  tick pg
  currentTime <- getCurrentTime
  let commandFinished = T.pack $ show currentTime
  return [commandFinished, T.concat ["moved ", T.pack input, " => ", T.pack output]]

sassCompiler :: ProgressBar -> Config -> Compiler
sassCompiler pg Config {sass_load_paths} = Compiler $ \input output -> do
  let loadPath = L.intercalate ":" sass_load_paths
  let sassc = "sassc " ++ input ++ " " ++ output ++ " --load-path " ++ loadPath -- <= include stuff that is needed in load-path
  runCmd pg sassc Nothing

runCmd :: ProgressBar -> String -> Maybe String -> Task [T.Text]
runCmd pg cmd maybeCwd = do
  -- TODO: handle exit status here
  (_, Just out, _, ph) <- lift $ createProcess (proc "bash" ["-c", cmd])
    { std_out = CreatePipe
    , cwd = maybeCwd
    }
  ec <- lift $ waitForProcess ph
  case ec of
      ExitSuccess   -> lift $ do
        content <- hGetContents out
        tick pg
        currentTime <- getCurrentTime
        let commandFinished = T.pack $ show currentTime
        return [commandFinished, T.pack cmd, T.pack content]
      ExitFailure _ -> throwError []
