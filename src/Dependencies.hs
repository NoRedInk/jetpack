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
module Dependencies
  ( find
  , Dependencies
  , Dependency(..)
  , FileType(..)
  , DependencyTree
  ) where

import Config
import Control.Applicative ((<|>))
import Control.Concurrent.Async.Lifted as Async
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe as M
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Tree as Tree
import Error
import GHC.Generics (Generic)
import qualified Parser.Ast as Ast
import qualified Parser.Import
import Parser.PackageJson as PackageJson
import qualified Parser.Require
import Safe
import qualified System.Directory as Dir
import System.FilePath
    ( makeRelative
    , takeDirectory
    , takeExtension
    , (<.>)
    , (</>)
    )
import System.FilePath.Glob (glob)
import System.Posix.Files
import Task (Task)
import Utils.Files (fileExistsTask, findAllFilesIn)
import Utils.Tree (searchNode)

data Dependency = Dependency
  { fileType             :: FileType
  , requiredAs           :: T.Text
  , filePath             :: FilePath
  , lastModificationTime :: Maybe UTCTime
  } deriving (Eq, Generic)

data FileType
  = EntryPoint Ast.SourceType
  | Child Ast.SourceType
  deriving (Eq, Generic, Show)


instance FromJSON Dependency
instance ToJSON Dependency
instance FromJSON FileType
instance ToJSON FileType

instance Show Dependency where
  show (Dependency t r p l) =
    "(Dependency: " ++ show r ++ " " ++ show t ++ " <" ++ show p ++ "> " ++ show l ++ ")"

type DependencyTree = Tree.Tree Dependency

type Dependencies = [DependencyTree]

{-| Find all dependencies for files in `module_directory`.
-}
find :: Config -> Task Dependencies
find config@Config {module_directory, temp_directory} = do
  depsJson <- lift $ BL.readFile $ temp_directory </> "deps" <.> "json"
  let cache = fromMaybe [] $ Aeson.decode depsJson :: Dependencies
  cwd <- lift Dir.getCurrentDirectory
  paths <- findAllFilesIn module_directory
  let relativePaths = fmap (makeRelative $ cwd </> module_directory) paths
  if null relativePaths
    then
      throwError [NoModulesPresent $ show module_directory]
    else do
      modules <- traverse (toDependency module_directory) relativePaths
      deps <- Async.forConcurrently modules (depsTree config cache)
      lift $ BL.writeFile (temp_directory </> "deps" <.> "json") $ Aeson.encode deps
      return deps

toDependency :: FilePath -> FilePath -> Task Dependency
toDependency module_directory path  = lift $ do
  status <- getFileStatus $ module_directory </> path
  let lastModificationTime = posixSecondsToUTCTime $ modificationTimeHiRes status
  return $ Dependency (EntryPoint Ast.Js) (T.pack path) path $ Just lastModificationTime

depsTree :: Config -> Dependencies -> Dependency -> Task DependencyTree
depsTree config cache = Tree.unfoldTreeM $ findRequires config cache

requireToDep :: FilePath -> Ast.Require -> Dependency
requireToDep path (Ast.Require t n) = Dependency (EntryPoint t) (T.pack n) path Nothing
requireToDep _ (Ast.Import n)    = Dependency (Child Ast.Elm) n ("." </> (T.unpack $ T.replace "." "/" n)) Nothing

updateDepPath :: FilePath -> Dependency -> Dependency
updateDepPath newPath (Dependency t r _ l) = Dependency t r newPath l

updateDepType :: Dependency -> Dependency
updateDepType (Dependency _ r p l) = Dependency newType r p l
  where newType = EntryPoint $ Parser.Require.getFileType $ takeExtension p

updateDepTime :: Dependency -> Task Dependency
updateDepTime (Dependency t r p _) = lift $ do
  status <- getFileStatus p
  let lastModificationTime = posixSecondsToUTCTime $ modificationTimeHiRes status
  return $ Dependency t r p $ Just lastModificationTime


findRequires :: Config -> Dependencies -> Dependency -> Task (Dependency, [Dependency])
findRequires config cache parent = do
  dep@Dependency {fileType} <- findRelative parent
    <|> findRelativeNodeModules parent
    <|> findInModules config parent
    <|> findInSources config parent
    <|> findInNodeModules config parent
    <|> findInRootNodeModules parent
    <|> findInVendorComponents parent
    <|> findInVendorJavascripts parent
    <|> globElm config parent
    <|> moduleNotFound config (requiredAs parent)
  case fileType of
    EntryPoint Ast.Js     -> parseModule cache dep Parser.Require.jsRequires
    EntryPoint Ast.Coffee -> parseModule cache dep Parser.Require.coffeeRequires
    EntryPoint Ast.Elm    -> parseModule cache dep Parser.Import.imports
    Child Ast.Elm         -> parseModule cache dep Parser.Import.imports
    _                     -> return (dep, [])

findInCache :: Dependency -> Dependencies -> Maybe (Dependency, [Dependency])
findInCache dep =
  headMay . M.catMaybes . fmap (findInCache_ dep)

findInCache_ :: Dependency -> DependencyTree -> Maybe (Dependency, [Dependency])
findInCache_ dep =
  fmap toTuple . searchNode (isSameDep dep . Tree.rootLabel)
  where toTuple Tree.Node{rootLabel, subForest} =
          (rootLabel, fmap Tree.rootLabel subForest)
        isSameDep :: Dependency -> Dependency -> Bool
        isSameDep a b =
          filePath a == filePath b
          && fileType a == fileType b
          && lastModificationTime a == lastModificationTime b

findRelative :: Dependency -> Task Dependency
findRelative parent =
  tryPlainJsExtAndIndex (filePath parent) (requiredAs parent) parent

findRelativeNodeModules :: Dependency -> Task Dependency
findRelativeNodeModules parent =
  tryPlainJsExtAndIndex
    (filePath parent </> "node_modules")
    (requiredAs parent)
    parent

findInModules :: Config -> Dependency -> Task Dependency
findInModules Config { module_directory } parent =
  tryPlainJsExtAndIndex module_directory (requiredAs parent) parent

findInSources :: Config -> Dependency -> Task Dependency
findInSources config parent =
  tryPlainJsExtAndIndex sourcePath (requiredAs parent) parent
  where
    sourcePath = Config.source_directory config

findInRootNodeModules :: Dependency -> Task Dependency
findInRootNodeModules parent =
  tryPlainJsExtAndIndex nodeModulesInRoot (requiredAs parent) parent
  where
    nodeModulesInRoot = "." </> "node_modules"

findInNodeModules :: Config -> Dependency -> Task Dependency
findInNodeModules config parent =
  tryPlainJsExtAndIndex nodeModulesPath (requiredAs parent) parent
  where
    nodeModulesPath = Config.source_directory config </> ".." </> "node_modules"

findInVendorComponents :: Dependency -> Task Dependency
findInVendorComponents parent =
  tryPlainJsExtAndIndex vendorComponentsPath (requiredAs parent) parent
  where
    vendorComponentsPath = "." </> "vendor" </> "assets" </> "components"

findInVendorJavascripts :: Dependency -> Task Dependency
findInVendorJavascripts parent =
  tryPlainJsExtAndIndex vendorJavaScriptsPath (requiredAs parent) parent
  where
    vendorJavaScriptsPath = "." </> "vendor" </> "assets" </> "javascripts"

globElm :: Config -> Dependency -> Task Dependency
globElm Config {source_directory} parent@Dependency {fileType, requiredAs} =
  case fileType of
    Child Ast.Elm -> do
      -- TODO use filepath
      let globi = T.concat [T.pack source_directory, "/**/", requiredAs, ".elm"]
      paths <- lift $ glob $ T.unpack globi
      -- TODO filter module == requiredAs
      case paths of
        [] -> throwError []
        x:_ -> do
          _ <- lift $ print requiredAs
          _ <- lift $ print x
          cwd <- lift Dir.getCurrentDirectory
          let p = T.pack $ makeRelative (cwd </> source_directory) x
          findInPath source_directory p parent
    _ -> throwError []

tryPlainJsExtAndIndex :: FilePath -> T.Text -> Dependency -> Task Dependency
tryPlainJsExtAndIndex basePath fileName require =
  -- check if we have a package.json.
  -- it contains information about the main file
  tryMainFromPackageJson basePath fileName require
  -- js
  <|> findInPath basePath "" require
  <|> findInPath basePath fileName require
  <|> findInPath basePath (T.concat [fileName, ".js"]) require
  <|> findInPath basePath (T.concat [fileName, "/index.js"]) require
  <|> findInPath basePath (T.concat [fileName, "/", fileName]) require
  <|> findInPath basePath (T.concat [fileName, "/", fileName, ".js"]) require
  -- coffeescript
  <|> findInPath basePath fileName require
  <|> findInPath basePath (T.concat [fileName, ".coffee"]) require
  <|> findInPath basePath (T.concat [fileName, "/index.coffee"]) require

tryMainFromPackageJson :: FilePath -> T.Text -> Dependency -> Task Dependency
tryMainFromPackageJson basePath f require = do
  let fileName = T.unpack f
  let packageJsonPath = basePath </> fileName </> "package" <.> "json"
  PackageJson maybeMain maybeBrowser <- PackageJson.load packageJsonPath
  case maybeBrowser <|> maybeMain of
    Just entryPoint ->
      findInPath basePath (T.pack (fileName </> T.unpack entryPoint)) require
    Nothing -> throwError []

moduleNotFound :: Config -> T.Text -> Task Dependency
moduleNotFound Config {module_directory, source_directory} fileName =
  throwError [ModuleNotFound module_directory source_directory $ show fileName]

findInPath :: FilePath -> T.Text -> Dependency -> Task Dependency
findInPath basePath path require = do
  let searchPath = basePath </> (T.unpack path)
  _ <- fileExistsTask searchPath
  updateDepTime $ updateDepType $ updateDepPath searchPath require

parseModule :: Dependencies -> Dependency -> (T.Text -> [Ast.Require]) -> Task (Dependency, [Dependency])
parseModule cache dep@Dependency {filePath} parser =
  case findInCache dep cache of
    Just cached -> return cached
    Nothing -> do
      content <- lift $ readFile filePath
      let requires = parser $ T.pack content
      let dependencies = fmap (requireToDep $ takeDirectory filePath) requires
      return (dep, dependencies)
