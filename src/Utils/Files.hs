{-# LANGUAGE OverloadedStrings #-}

{-| Helpers for working with files/paths/dirs)

### imports for doctests
    >>> :set -XOverloadedStrings
    >>> import System.FilePath ((</>), (<.>))
-}

module Utils.Files
  ( fileExistsTask
  , findAllFilesIn
  , pathToFileName
  , pathToFunctionName
  ) where

import Control.Monad.Except
import Control.Monad.Trans.Class (lift)
import Data.List as L
import Data.Text as T
import Error (Error (..))
import System.Directory (doesFileExist)
import System.FilePath (splitDirectories, (<.>), (</>))
import System.FilePath.Glob (glob)
import Task (Task)

{-| Checks if file exists and returns a failing task if it doesn't
-}
fileExistsTask :: FilePath -> Task ()
fileExistsTask path = do
  exists <- lift $ doesFileExist path
  case exists of
    True  -> lift $ return ()
    False -> throwError $ [FileNotFound (show path)]

{-| Returns a list of files in the given direcory and all it's subdirectories.
-}
findAllFilesIn :: FilePath -> Task [FilePath]
findAllFilesIn path = do
  let globi = path </> "**" </> "*.*"
  lift $ glob globi

{-| Converts a path into a flat filename.
    >>> pathToFileName ("." </> "foo" </> "bar" <.> "elm") "js"
    "foo___bar.elm.js"

    >>> pathToFileName ("." </> "bar" <.> "elm") "js"
    "bar.elm.js"
-}
pathToFileName :: FilePath -> String -> FilePath
pathToFileName filePath extension =
  newFileName <.> extension
  where
    newFileName =
      T.unpack
        $ T.concat
        $ L.intersperse "___"
        $ L.filter ((/=) ".")
        $ L.map T.pack
        $ splitDirectories filePath

pathToFunctionName :: FilePath -> String -> T.Text
pathToFunctionName filePath extension =
  T.replace "." "_" $ T.pack $ pathToFileName filePath extension
