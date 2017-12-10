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
  Config {entry_points} <- Task.getConfig
  Args {entryPointGlob} <- Task.getArgs
  let normalisedPattern = normalisePattern entry_points entryPointGlob
  paths <- findFilesIn normalisedPattern
  cwd <- toTask Dir.getCurrentDirectory
  case paths of
    [] -> throwError [NoModulesPresent $ show (takeDirectory normalisedPattern)]
    _ -> return $ (makeRelative $ cwd </> entry_points) <$> paths

normalisePattern :: FilePath -> Maybe FilePath -> FilePath
normalisePattern entryPointsRoot suppliedEntryPoints =
  case suppliedEntryPoints of
    Nothing ->
      entryPointsRoot </> "**" </> "*.*"
    Just entryPoints ->
      -- handle arguments with and without a leading "./"
      "." </> (normalise entryPoints)

findFilesIn :: FilePath -> Task [FilePath]
findFilesIn = toTask . glob
