{-| Setup working dir for jetpack.
-}
module Init where

import Config

import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))
import qualified ToolPaths

setup :: Config -> IO ToolPaths.ToolPaths
setup config = do
  let Config {tempDir, logDir, outputDir} = config
  requiredBins <- ToolPaths.find config
  _ <-
    traverse
      (createDirectoryIfMissing True)
      [tempDir, logDir, outputDir]
  createDepsJsonIfMissing tempDir
  return requiredBins

createDepsJsonIfMissing :: FilePath -> IO ()
createDepsJsonIfMissing tempDirectory = do
  let depsJSONPath = tempDirectory </> "deps" <.> "json"
  exists <- doesFileExist depsJSONPath
  if exists
    then return ()
    else TIO.writeFile depsJSONPath "[]"
