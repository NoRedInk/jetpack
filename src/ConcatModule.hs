{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Concat all modules required in an entrypoint into one file.
-}
module ConcatModule where

import Config
import qualified Control.Concurrent.Async.Lifted as Async
import Control.Monad.Trans.Class (lift)
import qualified Data.List.Utils as LU
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Tree as Tree
import Dependencies
import qualified Parser.Ast as Ast
import System.Directory (createDirectoryIfMissing)
import System.FilePath as FP
import Task
import Text.Regex (mkRegex, subRegex)
import qualified Utils.Files as F
import qualified Utils.Tree as UT

wrap :: Config -> Dependencies -> Task [FilePath]
wrap config dependencies =
  catMaybes <$> Async.mapConcurrently (wrapModules config) dependencies

wrapModules :: Config -> DependencyTree -> Task (Maybe FilePath)
wrapModules config dep = do
  wrapped <- traverse (wrapper config) $ uniqNodes dep
  writeModule config dep $ catMaybes wrapped

uniqNodes :: DependencyTree -> [(Dependency, [Dependency])]
uniqNodes = LU.uniq . UT.nodesWithChildren

wrapper :: Config -> (Dependency, [Dependency]) -> Task (Maybe T.Text)
wrapper Config {temp_directory} (d@Dependency {filePath}, ds) = lift $ do
  if compilesToJs d
    then do
      let name = F.pathToFileName filePath "js"
      content <- readFile $ temp_directory </> name
      let fnName = F.pathToFunctionName filePath "js"
      let replacedContent = foldr replaceRequire (T.pack content) ds
      let wrapped = wrapModule fnName replacedContent
      return $ Just wrapped
    else return Nothing

replaceRequire :: Dependency -> T.Text -> T.Text
replaceRequire Dependency {requiredAs, filePath} body =
  T.pack
  $ subRegex
    (mkRegex $ "require\\(['\"]" ++ requiredAs ++ "['\"]\\)")
    (T.unpack body) $ "jetpackRequire(" ++ fnName ++ ")"
  where fnName = T.unpack $ F.pathToFunctionName filePath "js"

compilesToJs :: Dependency -> Bool
compilesToJs Dependency { filePath, fileType } =
  case fileType of
    Ast.Js     -> FP.takeExtension filePath /= ".css"
    Ast.Elm    -> True
    Ast.Coffee -> True
    _          -> False

writeModule :: Config -> DependencyTree -> [T.Text] -> Task (Maybe FilePath)
writeModule Config { output_js_directory, module_directory} dependencyTree fns = lift $ do
  let Dependency {filePath} = Tree.rootLabel dependencyTree
  let outputPath = output_js_directory </> FP.makeRelative module_directory filePath
  let rootName = F.pathToFunctionName filePath "js"
  createDirectoryIfMissing True $ FP.takeDirectory outputPath
  writeFile outputPath $ T.unpack $ addBoilerplate rootName fns
  return $ Just outputPath

addBoilerplate :: T.Text -> [T.Text] -> T.Text
addBoilerplate root fns =
  T.unlines
  [ "(function() {"
  , "function objectAssign(target, varArgs) { // .length of function is 2"
  , "  'use strict';"
  , "  if (target == null) { // TypeError if undefined or null"
  , "    throw new TypeError('Cannot convert undefined or null to object');"
  , "  }"
  , "  var to = Object(target);"
  , "  for (var index = 1; index < arguments.length; index++) {"
  , "    var nextSource = arguments[index];"
  , "    if (nextSource != null) { // Skip over if undefined or null"
  , "      for (var nextKey in nextSource) {"
  , "        // Avoid bugs when hasOwnProperty is shadowed"
  , "        if (Object.prototype.hasOwnProperty.call(nextSource, nextKey)) {"
  , "          to[nextKey] = nextSource[nextKey];"
  , "        }"
  , "      }"
  , "    }"
  , "  }"
  , "  return to;"
  , "};"
  , "function jetpackRequire(fn) {"
  , "  var m = { exports : {}};"
  , "  var e = {};"
  , "  if (typeof fn !== \"function\") {"
  , "    console.error(\"Required function isn't a jetpack module.\", fn)"
  , "    return;"
  , "  }"
  , "  fn(m, e);  "
  , "  return objectAssign(m.exports, e);"
  , "}"
  , T.concat fns
  , T.concat ["jetpackRequire(", root, ");"] -- calling the entry point
  , "})();"
  ]

{-| Wraps a module in a function and injects require, module, exports.
    >>> :set -XOverloadedStrings
    >>> wrapModule "foo" "console.log(42);"
    "/* START: foo */\nfunction foo(module, exports) {\nconsole.log(42);} /* END: foo */\n"
-}
wrapModule :: T.Text -> T.Text -> T.Text
wrapModule _ "" = ""
wrapModule fnName body =
  T.concat
    [ "/* START: " , fnName , " */" , "\n"
    , T.concat ["function ", fnName, "(module, exports) {\n"]
    , body
    , "} /* END: " , fnName , " */"
    , "\n"
    ]
