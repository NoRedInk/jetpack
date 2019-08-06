{-| Helpers for working with files/paths/dirs)
-}
module Utils.Files
  ( pathToFileName
  )
where

import Protolude hiding ((<.>))
import qualified Data.List as L
import qualified Data.Text as T
import System.FilePath ((<.>), splitDirectories)
import Data.String

{-| Converts a path into a flat filename.
    >>> import System.FilePath ((</>), (<.>))
    >>> pathToFileName ("." </> "foo" </> "bar" <.> "elm") "js"
    "foo___bar.elm.js"

    >>> pathToFileName ("." </> "bar" <.> "elm") "js"
    "bar.elm.js"
-}
pathToFileName :: FilePath -> String -> FilePath
pathToFileName filePath extension = safeFileName filePath <.> extension

safeFileName :: FilePath -> FilePath
safeFileName =
  T.unpack .
    T.replace "-" "_" .
    T.concat .
    L.intersperse "___" .
    filter ((/=) ".") .
    fmap T.pack .
    splitDirectories
