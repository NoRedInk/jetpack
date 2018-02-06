{-| Setup working dir for jetpack.
-}
module Init where

import Config

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath
import Task (Task, lift)
import qualified ToolPaths

setup :: Config -> Task ToolPaths.ToolPaths
setup config = do
  let Config { temp_directory
             , log_directory
             , output_js_directory
             , output_css_directory
             } = config
  requiredBins <- ToolPaths.find config
  _ <-
    lift $
    traverse
      (createDirectoryIfMissing True)
      [temp_directory, log_directory, output_js_directory, output_css_directory]
  createDepsJsonIfMissing temp_directory
  return requiredBins

createDepsJsonIfMissing :: FilePath -> Task ()
createDepsJsonIfMissing tempDirectory =
  lift $ do
    let depsJSONPath = tempDirectory </> "deps" <.> "json"
    exists <- doesFileExist depsJSONPath
    if exists
      then return ()
      else writeFile depsJSONPath "[]"
