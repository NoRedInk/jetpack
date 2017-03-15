{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

module Init where

import Config
import Control.Monad.Trans.Class (lift)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath
import Task (Task)

setup :: Config -> Task ()
setup Config { temp_directory } = lift $ do
  _ <- createDirectoryIfMissing True temp_directory
  let depsJSONPath = temp_directory </> "deps" <.> "json"
  exists <- doesFileExist depsJSONPath
  if exists
     then return ()
     else writeFile depsJSONPath "[]"
