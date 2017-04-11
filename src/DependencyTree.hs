{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

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
module DependencyTree
  ( build
  , readTreeCache
  , writeTreeCache
  ) where

import Config
import Control.Monad.Trans.Class (lift)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe as M
import qualified Data.Text as T
import Data.Time.Clock ()
import Data.Time.Clock.POSIX
import qualified Data.Tree as Tree
import Dependencies
import qualified Parser.Ast as Ast
import Parser.PackageJson ()
import qualified Parser.Require
import qualified Resolver
import Safe
import System.Console.AsciiProgress
import System.FilePath (takeDirectory, (<.>), (</>))
import System.Posix.Files
import Task (Task)
import Utils.Tree (searchNode)

{-| Find all dependencies for the given entry points
-}
build :: ProgressBar -> Config -> Dependencies -> FilePath -> Task DependencyTree
build pg config cache entryPoint = do
  dependency <- toDependency (Config.module_directory config) entryPoint
  buildTree pg config cache dependency

buildTree :: ProgressBar -> Config -> Dependencies -> Dependency -> Task DependencyTree
buildTree pg config cache dep = do
  tree <- Tree.unfoldTreeM (findRequires config cache) dep
  lift $ tick pg
  return tree

readTreeCache :: FilePath -> Task Dependencies
readTreeCache tempDirectory = lift $ do
  depsJson <- BL.readFile $ tempDirectory </> "deps" <.> "json"
  return $ fromMaybe [] $ Aeson.decode depsJson

writeTreeCache :: FilePath -> Dependencies -> Task ()
writeTreeCache tempDirectory deps =
  lift $ BL.writeFile (tempDirectory </> "deps" <.> "json") $ Aeson.encode deps

toDependency :: FilePath -> FilePath -> Task Dependency
toDependency module_directory path  = lift $ do
  status <- getFileStatus $ module_directory </> path
  let lastModificationTime = posixSecondsToUTCTime $ modificationTimeHiRes status
  return $ Dependency Ast.Js path path $ Just lastModificationTime

requireToDep :: FilePath -> Ast.Require -> Dependency
requireToDep path (Ast.Require t n) = Dependency t n path Nothing
requireToDep _path (Ast.Import _n)  = undefined

findRequires :: Config -> Dependencies -> Dependency -> Task (Dependency, [Dependency])
findRequires config cache parent = do
  resolved <- Resolver.resolve config parent
  case fileType resolved of
    Ast.Js     -> parseModule cache resolved Parser.Require.jsRequires
    Ast.Coffee -> parseModule cache resolved Parser.Require.coffeeRequires
    Ast.Elm    -> return (resolved, [])
    Ast.Sass   -> return (resolved, [])

findInCache :: Dependency -> Dependencies -> Maybe (Dependency, [Dependency])
findInCache dep = headMay . M.catMaybes . fmap (findInCache_ dep)

findInCache_ :: Dependency -> DependencyTree -> Maybe (Dependency, [Dependency])
findInCache_ dep =
  fmap toTuple . searchNode ((==) dep . Tree.rootLabel)
  where toTuple Tree.Node{rootLabel, subForest} =
          (rootLabel, fmap Tree.rootLabel subForest)

parseModule :: Dependencies -> Dependency -> (T.Text -> [Ast.Require]) -> Task (Dependency, [Dependency])
parseModule cache dep@Dependency {filePath} parser =
  case findInCache dep cache of
    Just cached -> return cached
    Nothing -> do
      content <- lift $ readFile filePath
      let requires = parser $ T.pack content
      let dependencies = fmap (requireToDep $ takeDirectory filePath) requires
      return (dep, dependencies)
