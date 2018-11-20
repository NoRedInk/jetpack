{-| Setup working dir for jetpack.
-}
module Init where

import qualified Config
import Data.Foldable (traverse_)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))
import qualified ToolPaths

setup ::
     Config.TempDir
  -> Config.LogDir
  -> Config.OutputDir
  -> Maybe Config.ElmPath
  -> Maybe Config.CoffeePath
  -> IO ToolPaths.ToolPaths
setup tempDir logDir outputDir elmPath coffeePath = do
  requiredBins <- ToolPaths.find elmPath coffeePath
  traverse_
    (createDirectoryIfMissing True)
    [ Config.unTempDir tempDir
    , Config.unLogDir logDir
    , Config.unOutputDir outputDir
    ]
  createDepsJsonIfMissing tempDir
  return requiredBins

createDepsJsonIfMissing :: Config.TempDir -> IO ()
createDepsJsonIfMissing tempDir = do
  let depsJSONPath = Config.unTempDir tempDir </> "deps" <.> "json"
  exists <- doesFileExist depsJSONPath
  if exists
    then return ()
    else TIO.writeFile depsJSONPath "[]"
