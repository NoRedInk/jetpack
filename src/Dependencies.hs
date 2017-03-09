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
-}
module Dependencies
  ( find
  , Dependencies
  ) where

import Config
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import qualified Data.Tree as Tree
import Parser.Ast as Ast
import qualified Parser.Require
import System.Directory (doesFileExist)
import System.FilePath ((</>), dropFileName)
import Task (Task)
import Utils.Files (findAllFilesIn)

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
  paths <- findAllFilesIn $ Config.module_directory config
  let modules = fmap toDependency paths
  let sourcePath = Config.source_directory config
  traverse (depsTree sourcePath) modules

toDependency :: FilePath -> Dependency
toDependency path = Dependency Ast.Js path path

updatePath :: FilePath -> Dependency -> Dependency
updatePath path (Dependency t r _) = Dependency t r path

depsTree :: FilePath -> Dependency -> Task DependencyTree
depsTree sourcePath = Tree.unfoldTreeM $ findRequires sourcePath

requireToDep :: FilePath -> Ast.Require -> Dependency
requireToDep basePath (Ast.Require t n) = Dependency t n (basePath </> n)

findRequires :: FilePath -> Dependency -> Task (Dependency, [Dependency])
findRequires sourcePath require =
  lift $ do
    let path = sourcePath </> requiredAs require
    let basePath = dropFileName $ filePath require
    exists <- doesFileExist path
    if not exists
      then do
        let nodeModulesPath = sourcePath </> "node_modules"
        let nodeModule = nodeModulesPath </> requiredAs require
        exists <- doesFileExist nodeModule
        if not exists
          then return (require, [])
          else do
            content <- readFile nodeModule
            let requireNodeModule = updatePath nodeModule require
            return
              ( requireNodeModule
              , fmap (requireToDep nodeModulesPath) $
                Parser.Require.jsRequires $ T.pack content)
      else do
        content <- readFile path
        return
          ( require
          , fmap (requireToDep basePath) $
            Parser.Require.jsRequires $ T.pack content)
