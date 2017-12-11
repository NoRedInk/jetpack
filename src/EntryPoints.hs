{-| Finds all entrypoints. Find either uses a passed glob or **/*.* to search in the `entry_points`.
-}
module EntryPoints
  ( find
  ) where

import CliArguments (Args(..))
import Config
import Control.Monad.Except (throwError)

import Error
import qualified System.Directory as Dir
import System.FilePath ((</>), makeRelative, normalise, takeDirectory)
import "Glob" System.FilePath.Glob (glob)
import Task (Task, getArgs, getConfig, toTask)

find :: Task [FilePath]
find = do
  config <- Task.getConfig
  args <- Task.getArgs
  let entryPointsGlob = normalisedEntryPointsGlob config args
  paths <- findFilesIn entryPointsGlob
  cwd <- toTask Dir.getCurrentDirectory
  case paths of
    [] -> throwError [NoModulesPresent $ show (takeDirectory entryPointsGlob)]
    _ -> return $ (makeRelative $ cwd </> entry_points config) <$> paths

normalisedEntryPointsGlob :: Config -> Args -> FilePath
normalisedEntryPointsGlob config args =
    case entryPointGlob args of
      Nothing ->
        entry_points config </> "**" </> "*.*"
      Just entryPoints ->
        -- handle arguments with and without a leading "./"
        "." </> (normalise entryPoints)

findFilesIn :: FilePath -> Task [FilePath]
findFilesIn = toTask . glob
