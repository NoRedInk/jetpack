{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor     #-}
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
import Control.Concurrent.Async.Lifted as Async
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (left)
import qualified Data.Text as T
import qualified Data.Tree as Tree
import Error
import Parser.Ast as Ast
import Parser.PackageJson as PackageJson
import qualified Parser.Require
import System.Directory (doesFileExist)
import System.FilePath
    ( dropFileName
    , takeDirectory
    , takeExtension
    , (<.>)
    , (</>)
    )
import Task (Task)
import Utils.Files (fileExistsTask, findAllFilesIn)

data Dependency = Dependency
  { fileType   :: Ast.SourceType
  , requiredAs :: FilePath
  , filePath   :: FilePath
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
      Async.forConcurrently modules (depsTree config)

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

updateDepType :: Dependency -> Dependency
updateDepType (Dependency t r p) = Dependency newType r p
  where newType = Parser.Require.getFileType $ takeExtension p


findRequires :: Config -> Dependency -> Task (Dependency, [Dependency])
findRequires config parent = do
  findRelative config parent <|> findRelativeNodeModules config parent <|>
    findInModules config parent <|>
    findInSources config parent <|>
    findInNodeModules config parent <|>
    findInRootNodeModules config parent <|>
    findInVendorComponents config parent <|>
    findInVendorJavascripts config parent <|>
    moduleNotFound config (requiredAs parent)

findRelative :: Config -> Dependency -> Task (Dependency, [Dependency])
findRelative config parent =
  tryPlainJsExtAndIndex (filePath parent) (requiredAs parent) parent

findRelativeNodeModules :: Config -> Dependency -> Task (Dependency, [Dependency])
findRelativeNodeModules config parent =
  tryPlainJsExtAndIndex
    (filePath parent </> "node_modules")
    (requiredAs parent)
    parent

findInModules :: Config -> Dependency -> Task (Dependency, [Dependency])
findInModules config parent =
  tryPlainJsExtAndIndex modulesPath (requiredAs parent) parent
  where
    modulesPath = dropFileName $ filePath parent

findInSources :: Config -> Dependency -> Task (Dependency, [Dependency])
findInSources config parent =
  tryPlainJsExtAndIndex sourcePath (requiredAs parent) parent
  where
    sourcePath = Config.source_directory config

findInRootNodeModules :: Config -> Dependency -> Task (Dependency, [Dependency])
findInRootNodeModules config parent =
  tryPlainJsExtAndIndex nodeModulesInRoot (requiredAs parent) parent
  where
    nodeModulesInRoot = "." </> "node_modules"

findInNodeModules :: Config -> Dependency -> Task (Dependency, [Dependency])
findInNodeModules config parent =
  tryPlainJsExtAndIndex nodeModulesPath (requiredAs parent) parent
  where
    nodeModulesPath = Config.source_directory config </> ".." </> "node_modules"

findInVendorComponents :: Config -> Dependency -> Task (Dependency, [Dependency])
findInVendorComponents config parent =
  tryPlainJsExtAndIndex vendorComponentsPath (requiredAs parent) parent
  where
    vendorComponentsPath = "." </> "vendor" </> "assets" </> "components"

findInVendorJavascripts :: Config -> Dependency -> Task (Dependency, [Dependency])
findInVendorJavascripts config parent =
  tryPlainJsExtAndIndex vendorJavaScriptsPath (requiredAs parent) parent
  where
    vendorJavaScriptsPath = "." </> "vendor" </> "assets" </> "javascripts"

tryPlainJsExtAndIndex :: FilePath -> FilePath -> Dependency -> Task (Dependency, [Dependency])
tryPlainJsExtAndIndex basePath fileName require =
  -- check if we have a package.json.
  -- it contains information about the main file
  tryMainFromPackageJson basePath fileName require
  -- js
  <|> findJsInPath fileName
  <|> findJsInPath (fileName <.> "js")
  <|> findJsInPath (fileName </> "index.js")
  <|> findJsInPath (fileName </> fileName)
  <|> findJsInPath (fileName </> fileName <.> "js")
  -- coffeescript
  <|> findCoffeeInPath fileName
  <|> findCoffeeInPath (fileName <.> "coffee")
  <|> findCoffeeInPath (fileName </> "index.coffee")
  where
    findJsInPath f = findInPath Ast.Js basePath f require
    findCoffeeInPath f = findInPath Ast.Coffee basePath f require

tryMainFromPackageJson :: FilePath -> FilePath -> Dependency -> Task (Dependency, [Dependency])
tryMainFromPackageJson basePath fileName require = do
  let packageJsonPath = basePath </> fileName </> "package" <.> "json"
  PackageJson maybeMain maybeBrowser <- PackageJson.load packageJsonPath
  case maybeBrowser <|> maybeMain of
    Just entryPoint ->
      findInPath Ast.Js basePath (fileName </> T.unpack entryPoint) require
    Nothing -> left []

moduleNotFound :: Config -> FilePath -> Task (Dependency, [Dependency])
moduleNotFound (Config moduleDirectory sourceDirectory _ _ _) fileName = do
  left [ModuleNotFound moduleDirectory sourceDirectory $ show fileName]

findInPath :: Ast.SourceType -> FilePath -> FilePath -> Dependency -> Task (Dependency, [Dependency])
findInPath Ast.Js basePath path require = do parseModule basePath path require $ Parser.Require.jsRequires
findInPath Ast.Coffee basePath path require = do parseModule basePath path require $ Parser.Require.coffeeRequires
findInPath Ast.Elm basePath _ require = do return (require, [])

parseModule :: FilePath -> FilePath -> Dependency -> (T.Text -> [Ast.Require]) -> Task (Dependency, [Dependency])
parseModule basePath path require parser = do
  let searchPath = basePath </> path
  _ <- fileExistsTask searchPath
  content <- lift $ readFile searchPath
  let requires = parser $ T.pack content
  let dependencies = fmap (requireToDep $ takeDirectory searchPath) requires
  return (updateDepType $ updateDepPath searchPath require, dependencies)
