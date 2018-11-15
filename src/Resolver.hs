{-# LANGUAGE DeriveAnyClass #-}

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
module Resolver
  ( resolve
  ) where

import Alternative.IO (AlternativeIO)
import qualified Alternative.IO as AIO
import Config (Config)
import qualified Config
import Control.Applicative ((<|>))
import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import qualified Control.Monad.Except as ME
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Typeable (Typeable)
import Dependencies (Dependency(..))
import Parser.PackageJson as PackageJson
import qualified Parser.Require
import System.Directory (doesFileExist)
import System.FilePath ((<.>), (</>), takeExtension)
import System.Posix.Files

resolve :: Config -> Maybe Dependency -> Dependency -> IO Dependency
resolve config requiredIn dep = do
  result <- ME.runExceptT (resolveHelp config dep)
  case result of
    Left _ ->
      ES.throwM $ ModuleNotFound (filePath <$> requiredIn) $ requiredAs dep
    Right dep -> return dep

resolveHelp :: Config -> Dependency -> AlternativeIO Dependency
resolveHelp Config.Config { Config.modulesDirs
                          , Config.entryPoints
                          , Config.sourceDir
                          } dep = do
  resolved <-
    findRelative dep <|> findRelativeNodeModules dep <|>
    findInEntryPoints entryPoints dep <|>
    findInSources sourceDir dep <|>
    findInModules modulesDirs dep
  updateDepTime $ updateDepType resolved

findRelative :: Dependency -> AlternativeIO Dependency
findRelative dep@Dependency {filePath, requiredAs} =
  tryToFind filePath requiredAs dep

findRelativeNodeModules :: Dependency -> AlternativeIO Dependency
findRelativeNodeModules dep@Dependency {filePath, requiredAs} =
  tryToFind (filePath </> "node_modules") requiredAs dep

findInEntryPoints ::
     Config.EntryPoints -> Dependency -> AlternativeIO Dependency
findInEntryPoints entryPoints dep@Dependency {requiredAs} = do
  tryToFind (Config.unEntryPoints entryPoints) requiredAs dep

findInModules :: [FilePath] -> Dependency -> AlternativeIO Dependency
findInModules [] _parent = AIO.tryNext
findInModules (x:xs) dep@Dependency {requiredAs} =
  tryToFind x requiredAs dep <|> findInModules xs dep

findInSources :: FilePath -> Dependency -> AlternativeIO Dependency
findInSources sourceDir dep@Dependency {requiredAs} = do
  tryToFind sourceDir requiredAs dep

tryToFind :: FilePath -> FilePath -> Dependency -> AlternativeIO Dependency
tryToFind basePath fileName require = do
  let ext = takeExtension fileName
  case ext of
    ".js" -> tryJsWithExt basePath fileName require
    ".coffee" -> tryCoffeeWithExt basePath fileName require
    _ -> tryJs basePath fileName require <|> tryCoffee basePath fileName require

tryJs :: FilePath -> FilePath -> Dependency -> AlternativeIO Dependency
tryJs basePath fileName require =
  tryMainFromPackageJson basePath fileName require <|>
  moduleExistsInBase "" require <|>
  moduleExistsInBase fileName require <|>
  moduleExistsInBase (fileName <.> "js") require <|>
  moduleExistsInBase (fileName </> "index.js") require
  where
    moduleExistsInBase = moduleExists basePath

tryJsWithExt :: FilePath -> FilePath -> Dependency -> AlternativeIO Dependency
tryJsWithExt basePath fileName require =
  tryMainFromPackageJson basePath fileName require <|>
  moduleExistsInBase "" require <|>
  moduleExistsInBase fileName require
  where
    moduleExistsInBase = moduleExists basePath

tryCoffee :: FilePath -> FilePath -> Dependency -> AlternativeIO Dependency
tryCoffee basePath fileName require =
  moduleExistsInBase fileName require <|>
  moduleExistsInBase (fileName <.> "coffee") require <|>
  moduleExistsInBase (fileName </> "index.coffee") require
  where
    moduleExistsInBase = moduleExists basePath

tryCoffeeWithExt ::
     FilePath -> FilePath -> Dependency -> AlternativeIO Dependency
tryCoffeeWithExt basePath fileName require =
  moduleExistsInBase "" require <|> moduleExistsInBase fileName require
  where
    moduleExistsInBase = moduleExists basePath

{-| check if we have a package.json. It contains information about the main file.
-}
tryMainFromPackageJson ::
     FilePath -> FilePath -> Dependency -> AlternativeIO Dependency
tryMainFromPackageJson basePath fileName require = do
  let packageJsonPath = basePath </> fileName </> "package" <.> "json"
  exists <- AIO.lift (doesFileExist packageJsonPath)
  if exists
    then do
      PackageJson {main, browser} <- AIO.lift (PackageJson.load packageJsonPath)
      case browser <|> main of
        Just packageIndex ->
          moduleExists basePath (fileName </> packageIndex) require
        Nothing -> AIO.tryNext
    else AIO.tryNext

moduleExists :: FilePath -> FilePath -> Dependency -> AlternativeIO Dependency
moduleExists basePath path require = do
  let searchPath = basePath </> path
  exists <- AIO.lift (doesFileExist searchPath)
  if exists
    then return (require {filePath = searchPath})
    else AIO.tryNext

updateDepType :: Dependency -> Dependency
updateDepType (Dependency _ r p l) = Dependency newType r p l
  where
    newType = Parser.Require.getFileType $ takeExtension p

updateDepTime :: Dependency -> AlternativeIO Dependency
updateDepTime (Dependency t r p _) = do
  status <- AIO.lift (getFileStatus p)
  let lastModificationTime =
        posixSecondsToUTCTime $ modificationTimeHiRes status
  return $ Dependency t r p $ Just lastModificationTime

data Error =
  ModuleNotFound (Maybe FilePath)
                 FilePath
  deriving (Typeable, Exception)

instance Show Error where
  show (ModuleNotFound (Just requiredIn) file) =
    T.unpack $
    T.unlines
      [ ""
      , ""
      , "I had troubles finding '" <> T.pack file <> "' required in '" <>
        T.pack requiredIn <>
        "'."
      , ""
      , "Make sure that you spelled the name of the module correctly."
      , "You might also want to make sure that all dependencies are updated."
      ]
  show (ModuleNotFound Nothing file) =
    T.unpack $
    T.unlines
      ["", "", "I had troubles finding the entry point " <> T.pack file <> "."]
