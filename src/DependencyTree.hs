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


Finding modules
---------------

We are searching in the following folders.

1. relative to the file requiring the module
2. relative in node_modules
3. in `modules_directory`
4. in `source_directory`
5. in `{sourceDir}/../node_modules`
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
import Control.Monad ((<=<))
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
import ProgressBar (ProgressBar, pipeAndTick)
import qualified Resolver
import Safe
import System.FilePath ((<.>), (</>), takeDirectory)
import System.Posix.Files
import Task (Task, lift)
import Utils.Tree (searchNode)

{-| Find all dependencies for the given entry points
-}
build ::
     ProgressBar -> Config -> Dependencies -> FilePath -> Task DependencyTree
build pg config cache entryPoint =
  toDependency config entryPoint >>= buildTree config cache >>= pipeAndTick pg

buildTree :: Config -> Dependencies -> Dependency -> Task DependencyTree
buildTree config cache =
  Tree.unfoldTreeM (resolveChildren config <=< findRequires cache config) <=<
  Resolver.resolve config Nothing

readTreeCache :: FilePath -> IO Dependencies
readTreeCache tempDir =
  (fromMaybe [] . Aeson.decode) <$>
  BL.readFile (tempDir </> "deps" <.> "json")

writeTreeCache :: FilePath -> Dependencies -> IO ()
writeTreeCache tempDir =
  BL.writeFile (tempDir </> "deps" <.> "json") . Aeson.encode

toDependency :: Config -> FilePath -> Task Dependency
toDependency Config {entryPoints} path =
  lift $ do
    status <- getFileStatus $ entryPoints </> path
    let lastModificationTime =
          posixSecondsToUTCTime $ modificationTimeHiRes status
    return $ Dependency Ast.Js path path $ Just lastModificationTime

requireToDep :: FilePath -> Ast.Require -> Dependency
requireToDep path (Ast.Require t n) = Dependency t n path Nothing

findRequires ::
     Dependencies -> Config -> Dependency -> Task (Dependency, [Dependency])
findRequires cache Config {noParse} parent@Dependency {filePath, fileType} =
  if filePath `elem` noParse
    then return (parent, [])
    else case fileType of
           Ast.Js -> parseModule cache parent Parser.Require.jsRequires
           Ast.Coffee -> parseModule cache parent Parser.Require.coffeeRequires
           Ast.Elm -> return (parent, [])

findInCache :: Dependency -> Dependencies -> Maybe (Dependency, [Dependency])
findInCache dep = headMay . M.catMaybes . fmap (findInCache_ dep)

findInCache_ :: Dependency -> DependencyTree -> Maybe (Dependency, [Dependency])
findInCache_ dep = fmap toTuple . searchNode ((==) dep . Tree.rootLabel)
  where
    toTuple Tree.Node {Tree.rootLabel, Tree.subForest} =
      (rootLabel, fmap Tree.rootLabel subForest)

parseModule ::
     Dependencies
  -> Dependency
  -> (T.Text -> [Ast.Require])
  -> Task (Dependency, [Dependency])
parseModule cache dep@Dependency {filePath} parser =
  case findInCache dep cache of
    Just cached -> return cached
    Nothing -> do
      content <- lift $ readFile filePath
      let requires = parser $ T.pack content
      let dependencies = fmap (requireToDep $ takeDirectory filePath) requires
      return (dep, dependencies)

resolveChildren ::
     Config -> (Dependency, [Dependency]) -> Task (Dependency, [Dependency])
resolveChildren config (parent, children) = do
  resolved <- traverse (Resolver.resolve config (Just parent)) children
  return (parent, resolved)
