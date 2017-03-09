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


## Finding modules

We are searching in the following folders.

1. relative to the file requiring the module
2. relative in node_modules
3. in `modules_directory`
4. in `source_directory`
5. in `{source_directory}/../node_modules`
6. in `{root}/node_modules`
7. in `{root}/vendor/assets/components`
8. in `{root}/vendor/assets/javascripts`
9. woop woop! module not found

In each directory we search for the following names.
`{name}`  is the string from the `require` statement

1. `{folder}/{name}` from `browser` field in `package.json`
2. `{folder}/{name}` from `main` field in `package.json`
3. `{folder}/{name}`
4. `{folder}/{name}.js`
5. `{folder}/{name}/index.js`
6. `{folder}/{name}/{name}`
7. `{folder}/{name}/{name}.js`
8. `{folder}/{name}`
9. `{folder}/{name}.coffee`
10. `{folder}/{name}/index.coffee`

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
import Parser.PackageJson as PackageJson
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
findRequires config parent = do
  _ <- lift $ print parent
  let sourcePath = Config.source_directory config
  let requiredAs' = requiredAs parent
  let relativePath = filePath parent
  let modulesPath = dropFileName $ filePath parent
  let vendorComponentsPath = "." </> "vendor" </> "assets" </> "components"
  let vendorJavaScriptsPath = "." </> "vendor" </> "assets" </> "javascripts"
  let nodeModulesPath = sourcePath </> ".." </> "node_modules"
  let nodeModulesInRoot = "." </> "node_modules"
  let relativeRequire = tryPlainJsExtAndIndex relativePath requiredAs' parent
  let relativeNodeModulesRequire =
        tryPlainJsExtAndIndex
          (relativePath </> "node_modules")
          requiredAs'
          parent
  let moduleRequire = tryPlainJsExtAndIndex modulesPath requiredAs' parent
  let sourceRequire = tryPlainJsExtAndIndex sourcePath requiredAs' parent
  let nodeModuleRequireInRoot =
        tryPlainJsExtAndIndex nodeModulesInRoot requiredAs' parent
  let nodeModuleRequire =
        tryPlainJsExtAndIndex nodeModulesPath requiredAs' parent
  let vendorComponentsRequire =
        tryPlainJsExtAndIndex vendorComponentsPath requiredAs' parent
  let vendorJavaScriptsRequire =
        tryPlainJsExtAndIndex vendorJavaScriptsPath requiredAs' parent
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
  tryMain basePath fileName require <|>
  findInPath Ast.Js basePath fileName require <|>
  findInPath Ast.Js basePath (fileName <.> "js") require <|>
  findInPath Ast.Js basePath (fileName </> "index.js") require <|>
  findInPath Ast.Js basePath (fileName </> fileName) require <|>
  findInPath Ast.Js basePath (fileName </> fileName <.> "js") require <|>
  findInPath Ast.Coffee basePath fileName require <|>
  findInPath Ast.Coffee basePath (fileName <.> "coffee") require <|>
  findInPath Ast.Coffee basePath (fileName </> "index.coffee") require

tryMain :: FilePath -> FilePath -> Dependency -> Task (Dependency, [Dependency])
tryMain basePath fileName require = do
  PackageJson maybeMain maybeBrowser <-
    PackageJson.load $ basePath </> fileName </> "package" <.> "json"
  case maybeBrowser <|> maybeMain of
    Just browser ->
      findInPath Ast.Js basePath (fileName </> T.unpack browser) require
    Nothing -> left []

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
