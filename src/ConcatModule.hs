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
wrapper Config {temp_directory} (d@Dependency {filePath}, _ds) = lift $ do
  if compilesToJs d
    then do
      let name = F.pathToFileName filePath "js"
      content <- readFile $ temp_directory </> name
      let fnName = T.replace "." "_" $ T.pack $ name
      let wrapped = wrapModule fnName $ T.pack content
      return $ Just wrapped
    else return Nothing

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
  createDirectoryIfMissing True $ FP.takeDirectory outputPath
  writeFile outputPath $ T.unpack $ T.concat fns
  return $ Just outputPath

{-| Wraps a module in a function and injects require, module, exports.
    >>> :set -XOverloadedStrings
    >>> wrapModule "foo" "console.log(42);"
    "function foo(require, module, exports) {\nconsole.log(42);} /* END: foo */\n"
-}
wrapModule :: T.Text -> T.Text -> T.Text
wrapModule _ "" = ""
wrapModule fnName body =
  T.concat
    [ T.concat ["function ", fnName, "(require, module, exports) {\n"]
    , body
    , "} /* END: "
    , fnName
    , " */"
    , "\n"
    ]
