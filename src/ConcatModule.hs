{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module ConcatModule where

import Config as C
import Control.Monad.Trans.Class (lift)
import Data.List.Utils as LU
import Data.Maybe (catMaybes)
import Data.Text as T
import Data.Tree as Tree
import Dependencies as D
import Parser.Ast as Ast
import System.FilePath as FP
import Task
import Utils.Files as F

wrap :: C.Config -> D.Dependencies -> Task [FilePath]
wrap config dependencies = fmap catMaybes $ traverse (wrapModules config) dependencies


wrapModules :: C.Config -> D.DependencyTree -> Task (Maybe FilePath)
wrapModules C.Config { output_js_directory, temp_directory } dependencyTree = lift $ do
  let fileNames = catMaybes $ getCompiledDependencyFileNames dependencyTree
  modules <- traverse (\name -> readFile $ temp_directory </> name) fileNames
  case modules of
    [] -> return Nothing
    _ -> do
      let wrappedModules = fmap (wrapModule . T.pack) modules
      let root = Tree.rootLabel dependencyTree
      let outputPath = output_js_directory </> F.pathToFileName (D.filePath root) "js"
      writeFile outputPath $ T.unpack $ T.concat wrappedModules
      return $ Just outputPath


{-| Gets a unique list of compiled filenames from a dependency tree.
    getCompiledDependencyFileNames dependencyTree
    [Just "ui@@@src@@@index.js.js", Just "ui@@@src@@@Main.elm.js"]
-}
getCompiledDependencyFileNames :: D.DependencyTree -> [Maybe FP.FilePath]
getCompiledDependencyFileNames =
  fmap toFileName . LU.uniq . Tree.flatten
    where toFileName dep@Dependency { filePath } =
                case D.fileType dep of
                  Ast.Js ->
                    if FP.takeExtension filePath == ".css" then
                      Nothing
                    else
                      Just $ F.pathToFileName filePath "js"
                  Ast.Elm -> Just $ F.pathToFileName filePath "js"
                  Ast.Coffee -> Just $ F.pathToFileName filePath "js"
                  _      -> Nothing

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
