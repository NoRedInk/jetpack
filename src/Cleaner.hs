module Cleaner where

import Config
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Message
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))

clean :: Config.Config -> IO ()
clean Config {elm_root_directory, temp_directory} = do
  let elmStuff = (elm_root_directory </> "elm-stuff" </> "build-artifacts")
  removeDirectoryRecursive elmStuff
  Message.warning ("Removed " <> T.pack elmStuff)
  removeDirectoryRecursive temp_directory
  Message.warning ("Removed " <> T.pack temp_directory)
