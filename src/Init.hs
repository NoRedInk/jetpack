{-| Setup working dir for jetpack.
-}
module Init where

import Config

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))
import qualified ToolPaths

setup :: Config -> IO ToolPaths.ToolPaths
setup config = do
  let Config {temp_directory, log_directory, output_js_directory} = config
  requiredBins <- ToolPaths.find config
  _ <-
    traverse
      (createDirectoryIfMissing True)
      [temp_directory, log_directory, output_js_directory]
  createDepsJsonIfMissing temp_directory
  return requiredBins

createDepsJsonIfMissing :: FilePath -> IO ()
createDepsJsonIfMissing tempDirectory = do
  let depsJSONPath = tempDirectory </> "deps" <.> "json"
  exists <- doesFileExist depsJSONPath
  if exists
    then return ()
    else writeFile depsJSONPath "[]"
