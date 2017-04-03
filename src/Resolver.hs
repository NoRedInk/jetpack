{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Resolver (resolve) where

import Config (Config (..))
import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Dependencies (Dependency (..))
import Error (Error (ModuleNotFound))
import Parser.PackageJson as PackageJson
import qualified Parser.Require
import System.FilePath (takeExtension, (<.>), (</>))
import System.Posix.Files
import Task (Task)
import Utils.Files (fileExistsTask)

resolve :: Config -> Dependency -> Task Dependency
resolve config dep = do
  resolved <- findRelative dep
    <|> findRelativeNodeModules dep
    <|> findInModules config dep
    <|> findInSources config dep
    <|> findInVendorComponents dep
    <|> findInVendorJavascripts dep
    <|> findInNodeModules config dep
    <|> findInRootNodeModules dep
    <|> moduleNotFound config (requiredAs dep)
  updateDepTime $ updateDepType resolved

findRelative :: Dependency -> Task Dependency
findRelative parent =
  tryToFind (filePath parent) (requiredAs parent) parent

findRelativeNodeModules :: Dependency -> Task Dependency
findRelativeNodeModules parent =
  tryToFind (filePath parent </> "node_modules") (requiredAs parent) parent

findInModules :: Config -> Dependency -> Task Dependency
findInModules Config { module_directory } parent =
  tryToFind module_directory (requiredAs parent) parent

findInSources :: Config -> Dependency -> Task Dependency
findInSources config parent =
  tryToFind sourcePath (requiredAs parent) parent
  where
    sourcePath = Config.source_directory config

findInRootNodeModules :: Dependency -> Task Dependency
findInRootNodeModules parent =
  tryToFind nodeModulesInRoot (requiredAs parent) parent
  where
    nodeModulesInRoot = "." </> "node_modules"

findInNodeModules :: Config -> Dependency -> Task Dependency
findInNodeModules config parent =
  tryToFind nodeModulesPath (requiredAs parent) parent
  where
    nodeModulesPath = Config.source_directory config </> ".." </> "node_modules"

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
  <|> moduleExistsInBase (fileName </> fileName) require
  <|> moduleExistsInBase (fileName </> fileName <.> "js") require
  where moduleExistsInBase = moduleExists basePath

tryJsWithExt :: FilePath -> FilePath -> Dependency -> Task Dependency
tryJsWithExt basePath fileName require =
  tryMainFromPackageJson basePath fileName require
  <|> moduleExistsInBase "" require
  <|> moduleExistsInBase fileName require
  <|> moduleExistsInBase (fileName </> fileName) require
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

moduleNotFound :: Config -> FilePath -> Task Dependency
moduleNotFound Config {module_directory, source_directory} fileName =
  throwError [ModuleNotFound module_directory source_directory $ show fileName]

moduleExists :: FilePath -> FilePath -> Dependency -> Task Dependency
moduleExists basePath path require =
  fileExistsTask searchPath >> return (require { filePath = searchPath })
  where searchPath = basePath </> path

updateDepType :: Dependency -> Dependency
updateDepType (Dependency _ r p l) = Dependency newType r p l
  where newType = Parser.Require.getFileType $ takeExtension p

updateDepTime :: Dependency -> Task Dependency
updateDepTime (Dependency t r p _) = lift $ do
  status <- getFileStatus p
  let lastModificationTime = posixSecondsToUTCTime $ modificationTimeHiRes status
  return $ Dependency t r p $ Just lastModificationTime
