module Cleaner where

import Config
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Message
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))

clean :: Config.Config -> IO ()
clean Config {elmRoot, tempDir} = do
  let elmStuff = (elmRoot </> "elm-stuff" </> "build-artifacts")
  removeDirectoryRecursive elmStuff
  removeDirectoryRecursive tempDir
  Message.warning
    ("Removed " <> T.pack elmStuff <> " and " <> T.pack tempDir)
