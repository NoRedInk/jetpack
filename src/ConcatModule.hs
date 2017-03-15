{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module ConcatModule where

import Data.List.Utils as LU
import Data.Text as T
import Data.Tree as Tree
import Dependencies as D
import Utils.Files as F
import System.FilePath as FP


wrapModules :: D.DependencyTree -> T.Text
wrapModules =
    T.concat . fmap T.pack . getCompiledDependencyFileNames

{-| Gets a unique list of compiled filenames from a dependency tree.
    getCompiledDependencyFileNames dependencyTree
    ["ui@@@src@@@index.js.js", "ui@@@src@@@Main.elm.js"]
-}
getCompiledDependencyFileNames :: D.DependencyTree -> [FP.FilePath]
getCompiledDependencyFileNames =
    fmap ((\filePath -> F.pathToFileName filePath "js") . D.filePath) . LU.uniq . Tree.flatten

{-| Wraps a module in a function and injects require, module, exports.
    >>> :set -XOverloadedStrings
    >>> wrapModule "console.log(42);"
    "function(require, module, exports) {\nconsole.log(42);}\n"
-}
wrapModule :: T.Text -> T.Text
wrapModule "" = ""
wrapModule mod =
  T.concat
    [ "function(require, module, exports) {\n"
    , mod
    , "}\n"
    ]
