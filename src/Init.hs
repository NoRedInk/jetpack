{-# LANGUAGE OverloadedStrings #-}

{-| Setup working dir for jetpack.
-}
module Init where

import Config

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath
import Task (Task, getConfig, toTask)
import qualified ToolPaths

setup :: Task ToolPaths.ToolPaths
setup = do
  config@Config { temp_directory
                , log_directory
                , output_js_directory
                , output_css_directory
                } <- Task.getConfig
  requiredBins <- ToolPaths.find config
  _ <-
    toTask $
    traverse
      (createDirectoryIfMissing True)
      [temp_directory, log_directory, output_js_directory, output_css_directory]
  createDepsJsonIfMissing temp_directory
  return requiredBins

createDepsJsonIfMissing :: FilePath -> Task ()
createDepsJsonIfMissing tempDirectory =
  toTask $ do
    let depsJSONPath = tempDirectory </> "deps" <.> "json"
    exists <- doesFileExist depsJSONPath
    if exists
      then return ()
      else writeFile depsJSONPath "[]"
