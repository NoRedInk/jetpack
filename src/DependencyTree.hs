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
  ) where

import Config
import Control.Concurrent.Async.Lifted as Async
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
import System.FilePath (takeDirectory, takeExtension, (<.>), (</>))
import System.Posix.Files
import Task (Task)
import Utils.Tree (searchNode)

{-| Find all dependencies for files in `module_directory`.
-}
build :: Config -> [FilePath] -> Task Dependencies
build config@Config {module_directory, temp_directory} entryPoints = do
  cache <- readTreeCache temp_directory
  modules <- traverse (toDependency module_directory) entryPoints
  deps <- Async.forConcurrently modules (depsTree config cache)
  lift $ BL.writeFile (temp_directory </> "deps" <.> "json") $ Aeson.encode deps
  return deps

readTreeCache :: FilePath -> Task Dependencies
readTreeCache temp_directory = lift $ do
  depsJson <- BL.readFile $ temp_directory </> "deps" <.> "json"
  return $ fromMaybe [] $ Aeson.decode depsJson

toDependency :: FilePath -> FilePath -> Task Dependency
toDependency module_directory path  = lift $ do
  status <- getFileStatus $ module_directory </> path
  let lastModificationTime = posixSecondsToUTCTime $ modificationTimeHiRes status
  return $ Dependency Ast.Js path path $ Just lastModificationTime

depsTree :: Config -> Dependencies -> Dependency -> Task DependencyTree
depsTree config cache = Tree.unfoldTreeM $ findRequires config cache

requireToDep :: FilePath -> Ast.Require -> Dependency
requireToDep path (Ast.Require t n) = Dependency t n path Nothing
requireToDep _path (Ast.Import _n)  = undefined

updateDepType :: Dependency -> Dependency
updateDepType (Dependency _ r p l) = Dependency newType r p l
  where newType = Parser.Require.getFileType $ takeExtension p

updateDepTime :: Dependency -> Task Dependency
updateDepTime (Dependency t r p _) = lift $ do
  status <- getFileStatus p
  let lastModificationTime = posixSecondsToUTCTime $ modificationTimeHiRes status
  return $ Dependency t r p $ Just lastModificationTime

findRequires :: Config -> Dependencies -> Dependency -> Task (Dependency, [Dependency])
findRequires config cache parent = do
  resolved <- Resolver.resolve config parent
  dep <- updateDepTime $ updateDepType resolved
  case fileType dep of
    Ast.Js     -> parseModule cache dep Parser.Require.jsRequires
    Ast.Coffee -> parseModule cache dep Parser.Require.coffeeRequires
    Ast.Elm    -> return (dep, [])
    Ast.Sass   -> return (dep, [])

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
