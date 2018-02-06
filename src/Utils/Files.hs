{-| Helpers for working with files/paths/dirs)
-}
module Utils.Files
  ( fileExistsTask
  , pathToFileName
  , pathToFunctionName
  ) where

import Control.Monad.Except

import Data.List as L
import Data.Text as T
import Error (Error(..))
import System.Directory (doesFileExist)
import System.FilePath ((<.>), splitDirectories)
import Task (Task, lift)

{-| Checks if file exists and returns a failing task if it doesn't
-}
fileExistsTask :: FilePath -> Task ()
fileExistsTask path = do
  exists <- lift $ doesFileExist path
  case exists of
    True -> lift $ return ()
    False -> throwError $ [FileNotFound (show path)]

{-| Converts a path into a flat filename.
    >>> import System.FilePath ((</>), (<.>))
    >>> pathToFileName ("." </> "foo" </> "bar" <.> "elm") "js"
    "foo___bar.elm.js"

    >>> pathToFileName ("." </> "bar" <.> "elm") "js"
    "bar.elm.js"
-}
pathToFileName :: FilePath -> String -> FilePath
pathToFileName filePath extension = newFileName <.> extension
  where
    newFileName =
      T.unpack $
      T.replace "-" "_" $
      T.concat $
      L.intersperse "___" $
      L.filter ((/=) ".") $ L.map T.pack $ splitDirectories filePath

pathToFunctionName :: FilePath -> String -> T.Text
pathToFunctionName filePath extension =
  T.replace "@" "_" $ T.replace "." "_" $ T.pack $ pathToFileName filePath extension
