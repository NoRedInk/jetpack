{-| Helpers for working with files/paths/dirs)
-}
module Utils.Files
  ( pathToFileName
  , pathToFunctionName
  ) where

import Data.List as L
import Data.Text as T
import System.FilePath ((<.>), splitDirectories)

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
  T.replace "@" "_" $
  T.replace "." "_" $ T.pack $ pathToFileName filePath extension
