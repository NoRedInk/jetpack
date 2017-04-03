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

-- TODO check if require got replaced
replaceRequire :: Dependency -> T.Text -> T.Text
replaceRequire Dependency {requiredAs, filePath} body =
  T.pack $ subRegex requireRegex (T.unpack body) jetpackRequire
  where fnName = T.unpack $ F.pathToFunctionName filePath "js"
        requireRegex = mkRegex $ "require\\(['\"]" ++ requiredAs ++ "['\"]\\)"
        jetpackRequire = "jetpackRequire(" ++ fnName ++ ")"

compilesToJs :: Dependency -> Bool
compilesToJs Dependency { filePath, fileType } =
  case fileType of
    Ast.Js     -> FP.takeExtension filePath /= ".css"
    Ast.Elm    -> True
    Ast.Coffee -> True
    _          -> False

writeModule :: Config -> DependencyTree -> [T.Text] -> Task (Maybe FilePath)
writeModule config dependencyTree fns = do
  let root@Dependency { filePath } = Tree.rootLabel dependencyTree
  if compilesToJs root
     then writeJsModule config filePath fns
     else writeCssModule config filePath $ UT.roots $ Tree.subForest dependencyTree

writeJsModule :: Config -> FilePath -> [T.Text] -> Task (Maybe FilePath)
writeJsModule Config { output_js_directory, module_directory} rootFilePath fns = lift $ do
  let out = outputPath $ Output
              { outDir = output_js_directory
              , moduleDir = module_directory
              , name = rootFilePath
              }
  let rootName = F.pathToFunctionName rootFilePath "js"
  createDirectoryIfMissing True $ FP.takeDirectory out
  writeFile out $ T.unpack $ addBoilerplate rootName fns
  return $ Just out

writeCssModule :: Config -> FilePath -> [Dependency] -> Task (Maybe FilePath)
writeCssModule Config { output_css_directory, module_directory, temp_directory} rootFilePath deps = lift $ do
  let out = outputPath $ Output
              { outDir = output_css_directory
              , moduleDir = module_directory
              , name = rootFilePath
              }
  createDirectoryIfMissing True $ FP.takeDirectory out
  let cssPaths = fmap
                 ( (</>) temp_directory
                 . (flip F.pathToFileName "css")
                 . filePath
                 ) deps
  css <- traverse readFile cssPaths
  writeFile out $ T.unpack $ T.unlines $ fmap T.pack css
  return $ Just out


data Output = Output
  { outDir    :: FilePath
  , moduleDir :: FilePath
  , name      :: FilePath
  }

outputPath :: Output -> FilePath
outputPath Output { outDir, moduleDir, name } =
  outDir </> FP.makeRelative moduleDir name

addBoilerplate :: T.Text -> [T.Text] -> T.Text
addBoilerplate root fns =
  T.unlines
  [ "(function() {"
  , "function objectAssign(target, varArgs) {"
  , "  'use strict';"
  , "  if (target == null) {"
  , "    throw new TypeError('Cannot convert undefined or null to object');"
  , "  }"
  , "  var to = Object(target);"
  , "  for (var index = 1; index < arguments.length; index++) {"
  , "    var nextSource = arguments[index];"
  , "    if (nextSource != null) {"
  , "      for (var nextKey in nextSource) {"
  , "        if (Object.prototype.hasOwnProperty.call(nextSource, nextKey)) {"
  , "          to[nextKey] = nextSource[nextKey];"
  , "        }"
  , "      }"
  , "    }"
  , "  }"
  , "  return to;"
  , "};"
  , "var jetpackCache = {};"
  , "function jetpackRequire(fn) {"
  , "  var m = { exports : {}};"
  , "  var e = {};"
  , "  if (typeof fn !== \"function\") {"
  , "    console.error(\"Required function isn't a jetpack module.\", fn)"
  , "    return;"
  , "  }"
  , "  if (jetpackCache[fn.name]) {"
  , "    return jetpackCache[fn.name];"
  , "  }"
  , "  fn(m, e);  "
  , "  var mod = objectAssign(m.exports, e);"
  , "  jetpackCache[fn.name] = mod;"
  , "  return mod;"
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
