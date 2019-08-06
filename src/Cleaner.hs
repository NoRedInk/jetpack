module Cleaner where

import Config (Config (Config))
import qualified Config
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Message
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))

clean :: Config -> IO ()
clean Config {Config.elmRoot, Config.tempDir} = do
  let elmStuff =
        (Config.unElmRoot elmRoot </> "elm-stuff" </> "build-artifacts")
  removeDirectoryRecursive elmStuff
  removeDirectoryRecursive $ Config.unTempDir tempDir
  Message.warning
    ( "Removed " <> T.pack elmStuff <> " and " <>
      T.pack (Config.unTempDir tempDir)
    )
