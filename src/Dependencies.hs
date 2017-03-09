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
import System.FilePath ((</>), (<.>), dropFileName)
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

requireToDep :: Ast.Require -> Dependency
requireToDep (Ast.Require t n) = Dependency t n n

updateDepPath :: FilePath -> Dependency -> Dependency
updateDepPath newPath (Dependency t r p) = Dependency t r newPath

findRequires :: Config -> Dependency -> Task (Dependency, [Dependency])
findRequires config require = do
  let sourcePath = Config.source_directory config
  let requiredAs' = requiredAs require
  let modulesPath = dropFileName $ filePath require
  let nodeModulesPath = "" </> "node_modules"
  let nodeModulesPathInSourceDir = sourcePath </> nodeModulesPath
  let moduleRequire = tryPlainJsExtAndIndex modulesPath requiredAs' require
  let sourceRequire = tryPlainJsExtAndIndex sourcePath requiredAs' require
  let nodeModuleRequire =
        tryPlainJsExtAndIndex nodeModulesPathInSourceDir requiredAs' require
  let nodeModuleOneUpRequire =
        tryPlainJsExtAndIndex nodeModulesPath requiredAs' require
  moduleRequire <|> sourceRequire <|> nodeModuleOneUpRequire <|>
    nodeModuleRequire <|>
    moduleNotFound config requiredAs'

tryPlainJsExtAndIndex :: FilePath
                      -> FilePath
                      -> Dependency
                      -> Task (Dependency, [Dependency])
tryPlainJsExtAndIndex basePath fileName require =
  findInPath basePath fileName require <|>
  findInPath basePath (fileName <.> "js") require <|>
  findInPath basePath (fileName </> "index.js") require

moduleNotFound :: Config -> FilePath -> Task (Dependency, [Dependency])
moduleNotFound (Config moduleDirectory sourceDirectory _ _ _) fileName = do
  left [ModuleNotFound moduleDirectory sourceDirectory $ show fileName]

findInPath :: FilePath
           -> FilePath
           -> Dependency
           -> Task (Dependency, [Dependency])
findInPath basePath path require = do
  let searchPath = basePath </> path
  _ <- fileExistsTask searchPath
  content <- lift $ readFile searchPath
  let requires = Parser.Require.jsRequires $ T.pack content
  let dependencies = fmap requireToDep requires
  return (updateDepPath searchPath require, dependencies)
