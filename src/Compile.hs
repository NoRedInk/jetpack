{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
-}
module Compile where

import Config (Config (..))
import Control.Monad.Except (throwError)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List as L
import Data.List.Utils (uniq)
import qualified Data.Map.Strict as Map
import Data.Text as T
import Data.Time.Clock
import Data.Tree as Tree
import Dependencies (Dependencies, Dependency (..))
import Env
import GHC.IO.Handle
import Parser.Ast as Ast
import qualified ProgressBar
import System.Directory (copyFile)
import qualified System.Directory as Dir
import System.Exit
import System.FilePath ((<.>), (</>))
import System.Process
import Task (Task, getArgs, getConfig, toTask)
import ToolPaths
import Utils.Files (pathToFileName)

newtype Compiler = Compiler { runCompiler :: FilePath -> FilePath -> Task [T.Text] }

compile :: ToolPaths ->  Dependency -> Task T.Text
compile toolPaths Dependency {fileType, filePath} = do
  config <- Task.getConfig
  let (c, outputType) = compiler fileType config toolPaths
  let outputPath = buildArtifactPath config outputType filePath
  log <- (runCompiler c) filePath outputPath
  return $ T.unlines log

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
  Args {debug} <- Task.getArgs
  let debugFlag =
        if debug then
          " --debug --yes"
        else
          " --yes"
  let cmd = elmMake ++ " " ++ "../" ++ input ++ " --output " ++ "../" ++ output ++ debugFlag
  runCmd cmd $ Just elm_root_directory

coffeeCompiler :: FilePath -> Compiler
coffeeCompiler coffee = Compiler $ \input output -> do
  let cmd = coffee ++ " -p " ++ input ++ " > " ++ output
  runCmd cmd Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: Compiler
jsCompiler = Compiler $ \input output -> do
  _ <- toTask $ copyFile input output
  _ <- ProgressBar.step
  currentTime <- toTask getCurrentTime
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
  (_, Just out, _, ph) <- toTask $ createProcess (proc "bash" ["-c", cmd])
    { std_out = CreatePipe
    , cwd = maybeCwd
    }
  ec <- toTask $ waitForProcess ph
  case ec of
      ExitSuccess   -> do
        content <- toTask $ hGetContents out
        _ <- ProgressBar.step
        currentTime <- toTask $ getCurrentTime
        let commandFinished = T.pack $ show currentTime
        return [commandFinished, T.pack cmd, T.pack content]
      ExitFailure _ -> throwError []


whatNeedsCompilation :: Dependencies -> Task [Dependency]
whatNeedsCompilation deps = do
  let uniqModules = uniq $ L.concatMap Tree.flatten deps
  lastCompiled <- readModuleJson
  return $ L.filter (filterByIfAlreadyCompiled lastCompiled) uniqModules

filterByIfAlreadyCompiled :: CompiledModules -> Dependency -> Bool
filterByIfAlreadyCompiled compiledModules dep =
  case (fileType dep, lastMod) of
    (Ast.Elm, _)      -> True
    (Ast.Sass, _)     -> True
    (_, Just lastMod) -> lastMod /= lastModificationTime dep
    (_, Nothing)      -> True
  where lastMod = fmap snd $ Map.lookup (filePath dep) compiledModules

type CompiledModules = Map.Map FilePath (UTCTime, Maybe UTCTime)

modulesJsonPath :: Task FilePath
modulesJsonPath = do
  Config {temp_directory} <- Task.getConfig
  return $ temp_directory </> "modules" <.> "json"

createdModulesJson :: [Dependency] -> Task ()
createdModulesJson deps = do
  compileTime <- toTask getCurrentTime
  jsonPath <- modulesJsonPath
  exists <- toTask $ Dir.doesFileExist jsonPath
  if exists
    then do
      modules <- readModuleJson
      updateModulesJson jsonPath deps compileTime modules
    else
      updateModulesJson jsonPath deps compileTime (Map.empty)

updateModulesJson :: FilePath -> [Dependency] -> UTCTime -> CompiledModules -> Task ()
updateModulesJson modulesJsonPath deps compileTime modules = do
  let newContent = L.foldl (\acc k -> Map.insert (filePath k) (compileTime, lastModificationTime k) acc) modules deps
  let encodedPaths = Aeson.encode newContent
  _ <- toTask $ BL.writeFile modulesJsonPath encodedPaths
  _ <- ProgressBar.step
  return ()

readModuleJson :: Task CompiledModules
readModuleJson = do
  jsonPath <- modulesJsonPath
  content <- toTask $ BL.readFile jsonPath
  case Aeson.decode content of
    Just modules -> return modules
    Nothing      -> return Map.empty
