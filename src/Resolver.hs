{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

{-| Resolves `requires`-statements. It tries to locate the module in the following directories.

1. relative to the file requiring the module
2. relative in node_modules
3. in `modules_directory`
4. in `source_directory`
7. in `{root}/vendor/assets/components`
8. in `{root}/vendor/assets/javascripts`
5. in `{source_directory}/../node_modules`
6. in `{root}/node_modules`
9. woop woop! module not found

In each directory we search for the following names.
`{name}`  is the string from the `require` statement

1. `{folder}/{name}` from `browser` field in `package.json`
2. `{folder}/{name}` from `main` field in `package.json`
3. `{folder}/{name}`
4. `{folder}/{name}.js`
5. `{folder}/{name}/index.js`
6. `{folder}/{name}`
7. `{folder}/{name}.coffee`
8. `{folder}/{name}/index.coffee`

-}
module Resolver (resolve) where

import Config (Config (..))
import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)

import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Dependencies (Dependency (..))
import Error (Error (ModuleNotFound))
import Parser.PackageJson as PackageJson
import qualified Parser.Require
import System.FilePath (takeExtension, (<.>), (</>))
import System.Posix.Files
import Task (Task, getConfig, toTask)
import Utils.Files (fileExistsTask)

resolve :: Dependency -> Task Dependency
resolve dep = do
  resolved <- findRelative dep
    <|> findRelativeNodeModules dep
    <|> findInModules dep
    <|> findInSources dep
    <|> findInVendorComponents dep
    <|> findInVendorJavascripts dep
    <|> findInNodeModules dep
    <|> findInRootNodeModules dep
    <|> moduleNotFound (requiredAs dep)
  updateDepTime $ updateDepType resolved

findRelative :: Dependency -> Task Dependency
findRelative parent =
  tryToFind (filePath parent) (requiredAs parent) parent

findRelativeNodeModules :: Dependency -> Task Dependency
findRelativeNodeModules parent =
  tryToFind (filePath parent </> "node_modules") (requiredAs parent) parent

findInModules :: Dependency -> Task Dependency
findInModules parent = do
  Config {entry_points} <- Task.getConfig
  tryToFind entry_points (requiredAs parent) parent

findInSources :: Dependency -> Task Dependency
findInSources parent = do
  Config {source_directory} <- Task.getConfig
  tryToFind source_directory (requiredAs parent) parent

findInRootNodeModules :: Dependency -> Task Dependency
findInRootNodeModules parent =
  tryToFind nodeModulesInRoot (requiredAs parent) parent
  where
    nodeModulesInRoot = "." </> "node_modules"

findInNodeModules :: Dependency -> Task Dependency
findInNodeModules parent = do
  Config {source_directory} <- Task.getConfig
  let nodeModulesPath = source_directory </> ".." </> "node_modules"
  tryToFind nodeModulesPath (requiredAs parent) parent

findInVendorComponents :: Dependency -> Task Dependency
findInVendorComponents parent =
  tryToFind vendorComponentsPath (requiredAs parent) parent
  where
    vendorComponentsPath = "." </> "vendor" </> "assets" </> "components"

findInVendorJavascripts :: Dependency -> Task Dependency
findInVendorJavascripts parent =
  tryToFind vendorJavaScriptsPath (requiredAs parent) parent
  where
    vendorJavaScriptsPath = "." </> "vendor" </> "assets" </> "javascripts"

tryToFind :: FilePath -> FilePath -> Dependency -> Task Dependency
tryToFind basePath fileName require = do
  let ext = takeExtension fileName
  case ext of
    ".js" -> tryJsWithExt basePath fileName require
    ".coffee" -> tryCoffeeWithExt basePath fileName require
    _ -> tryJs basePath fileName require <|> tryCoffee basePath fileName require

tryJs :: FilePath -> FilePath -> Dependency -> Task Dependency
tryJs basePath fileName require =
  tryMainFromPackageJson basePath fileName require
  <|> moduleExistsInBase "" require
  <|> moduleExistsInBase fileName require
  <|> moduleExistsInBase (fileName <.> "js") require
  <|> moduleExistsInBase (fileName </> "index.js") require
  where moduleExistsInBase = moduleExists basePath

tryJsWithExt :: FilePath -> FilePath -> Dependency -> Task Dependency
tryJsWithExt basePath fileName require =
  tryMainFromPackageJson basePath fileName require
  <|> moduleExistsInBase "" require
  <|> moduleExistsInBase fileName require
  where moduleExistsInBase = moduleExists basePath

tryCoffee :: FilePath -> FilePath -> Dependency -> Task Dependency
tryCoffee basePath fileName require =
  moduleExistsInBase fileName require
  <|> moduleExistsInBase (fileName <.> "coffee") require
  <|> moduleExistsInBase (fileName </> "index.coffee") require
  where moduleExistsInBase = moduleExists basePath

tryCoffeeWithExt :: FilePath -> FilePath -> Dependency -> Task Dependency
tryCoffeeWithExt basePath fileName require =
  moduleExistsInBase "" require
  <|> moduleExistsInBase fileName require
  where moduleExistsInBase = moduleExists basePath


{-| check if we have a package.json. It contains information about the main file.
-}
tryMainFromPackageJson :: FilePath -> FilePath -> Dependency -> Task Dependency
tryMainFromPackageJson basePath fileName require = do
  let packageJsonPath = basePath </> fileName </> "package" <.> "json"
  PackageJson maybeMain maybeBrowser <- PackageJson.load packageJsonPath
  case maybeBrowser <|> maybeMain of
    Just entryPoint ->
      moduleExists basePath (fileName </> T.unpack entryPoint) require
    Nothing -> throwError []

moduleNotFound :: FilePath -> Task Dependency
moduleNotFound fileName = do
  Config {source_directory, entry_points} <- Task.getConfig
  throwError [ModuleNotFound entry_points source_directory $ show fileName]

moduleExists :: FilePath -> FilePath -> Dependency -> Task Dependency
moduleExists basePath path require =
  fileExistsTask searchPath >> return (require { filePath = searchPath })
  where searchPath = basePath </> path

updateDepType :: Dependency -> Dependency
updateDepType (Dependency _ r p l) = Dependency newType r p l
  where newType = Parser.Require.getFileType $ takeExtension p

updateDepTime :: Dependency -> Task Dependency
updateDepTime (Dependency t r p _) = toTask $ do
  status <- getFileStatus p
  let lastModificationTime = posixSecondsToUTCTime $ modificationTimeHiRes status
  return $ Dependency t r p $ Just lastModificationTime
