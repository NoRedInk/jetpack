{-| Setup working dir for jetpack.
-}
module Init where

import qualified Config
import Config (Config(Config))

import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))
import qualified ToolPaths

setup :: Config -> IO ToolPaths.ToolPaths
setup config = do
  let Config {Config.tempDir, Config.logDir, Config.outputDir} = config
  requiredBins <- ToolPaths.find config
  _ <-
    traverse
      (createDirectoryIfMissing True)
      [Config.unTempDir tempDir, Config.unLogDir logDir, outputDir]
  createDepsJsonIfMissing tempDir
  return requiredBins

createDepsJsonIfMissing :: Config.TempDir -> IO ()
createDepsJsonIfMissing tempDir = do
  let depsJSONPath = Config.unTempDir tempDir </> "deps" <.> "json"
  exists <- doesFileExist depsJSONPath
  if exists
    then return ()
    else TIO.writeFile depsJSONPath "[]"
