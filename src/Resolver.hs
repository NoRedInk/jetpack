{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Resolver where

import Config
import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Dependencies (Dependency (..))
import Error
import Parser.PackageJson as PackageJson
import System.FilePath ((<.>), (</>))
import Task (Task)
import Utils.Files (fileExistsTask)

findRequires :: Config -> Dependency -> Task Dependency
findRequires config parent = do
  findRelative parent
    <|> findRelativeNodeModules parent
    <|> findInModules config parent
    <|> findInSources config parent
    <|> findInNodeModules config parent
    <|> findInRootNodeModules parent
    <|> findInVendorComponents parent
    <|> findInVendorJavascripts parent
    <|> moduleNotFound config (requiredAs parent)

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

tryPlainJsExtAndIndex :: FilePath -> FilePath -> Dependency -> Task Dependency
tryPlainJsExtAndIndex basePath fileName require =
  -- check if we have a package.json.
  -- it contains information about the main file
  tryMainFromPackageJson basePath fileName require
  -- js
  <|> moduleExists basePath "" require
  <|> moduleExists basePath fileName require
  <|> moduleExists basePath (fileName <.> "js") require
  <|> moduleExists basePath (fileName </> "index.js") require
  <|> moduleExists basePath (fileName </> fileName) require
  <|> moduleExists basePath (fileName </> fileName <.> "js") require
  -- coffeescript
  <|> moduleExists basePath fileName require
  <|> moduleExists basePath (fileName <.> "coffee") require
  <|> moduleExists basePath (fileName </> "index.coffee") require

tryMainFromPackageJson :: FilePath -> FilePath -> Dependency -> Task Dependency
tryMainFromPackageJson basePath fileName require = do
  let packageJsonPath = basePath </> fileName </> "package" <.> "json"
  PackageJson maybeMain maybeBrowser <- PackageJson.load packageJsonPath
  case maybeBrowser <|> maybeMain of
    Just entryPoint ->
      moduleExists basePath (fileName </> T.unpack entryPoint) require
    Nothing -> throwError []

moduleNotFound :: Config -> FilePath -> Task Dependency
moduleNotFound Config {module_directory, source_directory} fileName =
  throwError [ModuleNotFound module_directory source_directory $ show fileName]

moduleExists :: FilePath -> FilePath -> Dependency -> Task Dependency
moduleExists basePath path require =
  fileExistsTask searchPath >> return (updateDepPath searchPath require)
  where searchPath = basePath </> path

updateDepPath :: FilePath -> Dependency -> Dependency
updateDepPath newPath (Dependency t r _ l) = Dependency t r newPath l
