{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Finds all dependencies of a module. It creates a try like the following for each module.
  ```
  (Dependency "./app/assets/modules/js/Super/foo_bar.js" Js <fullpath>)
  |
  +- (Dependency "Page/Super/FooBar/Main.elm" Elm <fullpath>)
  |
  `- (Dependency "Page/Super/FooBar/index.coffee" Coffee <fullpath>)
    |
    `- (Dependency "lodash" Js)
  ```


## The path problem:

We need the absolute path as well as the path that a file got required by.
The absolute path is different, for each model.
Entry points: Config.module_directory / filename
Sourcefile: Config.source_directory / filename
Sourcefile: Config.source_directory / name /index.js
NodeModule: Config.source_directory / node_modules / filename
NodeModule: Config.source_directory / node_modules / name / index.js
NodeModule: Config.source_directory / .. / node_modules / filename
NodeModule: Config.source_directory / .. / node_modules / name / index.js
-}
module Dependencies
  ( find
  , Dependencies
  , Dependency(..)
  ) where

import Config
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (left)
import qualified Data.Text as T
import qualified Data.Tree as Tree
import Error
import Parser.Ast as Ast
import qualified Parser.Require
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>), dropFileName, takeDirectory)
import Task (Task)
import Utils.Files (findAllFilesIn, fileExistsTask)

data Dependency = Dependency
  { fileType :: Ast.SourceType
  , requiredAs :: FilePath
  , filePath :: FilePath
  } deriving (Eq)

instance Show Dependency where
  show (Dependency t r p) =
    "(Dependency: " ++ show r ++ " " ++ show t ++ " <" ++ show p ++ ">)"

type DependencyTree = Tree.Tree Dependency

type Dependencies = [DependencyTree]

{-| Find all dependencies for files in `module_directory`.
-}
find :: Config -> Task Dependencies
find config = do
  let modulesPath = Config.module_directory config
  paths <- findAllFilesIn modulesPath
  if paths == []
    then do
      left [NoModulesPresent $ show modulesPath]
    else do
      let modules = fmap toDependency paths
      traverse (depsTree config) modules

toDependency :: FilePath -> Dependency
toDependency path = Dependency Ast.Js path path

updatePath :: FilePath -> Dependency -> Dependency
updatePath path (Dependency t r _) = Dependency t r path

depsTree :: Config -> Dependency -> Task DependencyTree
depsTree config = Tree.unfoldTreeM $ findRequires config

requireToDep :: FilePath -> Ast.Require -> Dependency
requireToDep path (Ast.Require t n) = Dependency t n path

updateDepPath :: FilePath -> Dependency -> Dependency
updateDepPath newPath (Dependency t r p) = Dependency t r newPath

findRequires :: Config -> Dependency -> Task (Dependency, [Dependency])
findRequires config require = do
  let sourcePath = Config.source_directory config
  let requiredAs' = requiredAs require
  let relativePath = filePath require
  let modulesPath = dropFileName $ filePath require
  let vendorComponentsPath = "." </> "vendor" </> "assets" </> "components"
  let vendorJavaScriptsPath = "." </> "vendor" </> "assets" </> "javascripts"
  let nodeModulesPath = sourcePath </> ".." </> "node_modules"
  let nodeModulesInRoot = "." </> "node_modules"
  let relativeRequire = tryPlainJsExtAndIndex relativePath requiredAs' require
  let relativeNodeModulesRequire =
        tryPlainJsExtAndIndex
          (relativePath </> "node_modules")
          requiredAs'
          require
  let moduleRequire = tryPlainJsExtAndIndex modulesPath requiredAs' require
  let sourceRequire = tryPlainJsExtAndIndex sourcePath requiredAs' require
  let nodeModuleRequireInRoot =
        tryPlainJsExtAndIndex nodeModulesInRoot requiredAs' require
  let nodeModuleRequire =
        tryPlainJsExtAndIndex nodeModulesPath requiredAs' require
  let vendorComponentsRequire =
        tryPlainJsExtAndIndex vendorComponentsPath requiredAs' require
  let vendorJavaScriptsRequire =
        tryPlainJsExtAndIndex vendorJavaScriptsPath requiredAs' require
  relativeRequire <|> relativeNodeModulesRequire <|> moduleRequire <|>
    sourceRequire <|>
    nodeModuleRequire <|>
    nodeModuleRequireInRoot <|>
    vendorComponentsRequire <|>
    vendorJavaScriptsRequire <|>
    moduleNotFound config requiredAs'

tryPlainJsExtAndIndex :: FilePath
                      -> FilePath
                      -> Dependency
                      -> Task (Dependency, [Dependency])
tryPlainJsExtAndIndex basePath fileName require =
  findInPath Ast.Js basePath fileName require <|>
  findInPath Ast.Js basePath (fileName <.> "js") require <|>
  findInPath Ast.Js basePath (fileName </> "index.js") require <|>
  findInPath Ast.Js basePath (fileName </> fileName) require <|>
  findInPath Ast.Js basePath (fileName </> fileName <.> "js") require <|>
  findInPath Ast.Coffee basePath fileName require <|>
  findInPath Ast.Coffee basePath (fileName <.> "coffee") require <|>
  findInPath Ast.Coffee basePath (fileName </> "index.coffee") require

moduleNotFound :: Config -> FilePath -> Task (Dependency, [Dependency])
moduleNotFound (Config moduleDirectory sourceDirectory _ _ _) fileName = do
  left [ModuleNotFound moduleDirectory sourceDirectory $ show fileName]

findInPath
  :: Ast.SourceType
  -> FilePath
  -> FilePath
  -> Dependency
  -> Task (Dependency, [Dependency])
findInPath Ast.Js basePath path require = do
  parseModule basePath path require $ Parser.Require.jsRequires
findInPath Ast.Coffee basePath path require = do
  parseModule basePath path require $ Parser.Require.coffeeRequires
findInPath Ast.Elm basePath path require = do
  return (require, [])

parseModule
  :: FilePath
  -> FilePath
  -> Dependency
  -> (T.Text -> [Ast.Require])
  -> Task (Dependency, [Dependency])
parseModule basePath path require parser = do
  let searchPath = basePath </> path
  _ <- fileExistsTask searchPath
  content <- lift $ readFile searchPath
  let requires = parser $ T.pack content
  let dependencies = fmap (requireToDep $ takeDirectory searchPath) requires
  return (updateDepPath searchPath require, dependencies)
