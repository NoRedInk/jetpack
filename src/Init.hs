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
  let Config {tempDir, logDir, outputDir} = config
  requiredBins <- ToolPaths.find config
  _ <-
    lift $
    traverse
      (createDirectoryIfMissing True)
      [tempDir, logDir, outputDir]
  createDepsJsonIfMissing tempDir
  return requiredBins

createDepsJsonIfMissing :: FilePath -> Task ()
createDepsJsonIfMissing tempDirectory =
  lift $ do
    let depsJSONPath = tempDirectory </> "deps" <.> "json"
    exists <- doesFileExist depsJSONPath
    if exists
      then return ()
      else writeFile depsJSONPath "[]"
