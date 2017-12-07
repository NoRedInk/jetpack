{-# LANGUAGE PackageImports #-}

{-| Finds all entrypoints. Find either uses a passed glob or **/*.* to search in the `entry_points`.
-}
module EntryPoints
  ( find
  ) where

import CliArguments (Args(..))
import Config
import Control.Monad.Except (throwError)

import qualified Data.Maybe as M
import Error
import qualified System.Directory as Dir
import System.FilePath ((</>), makeRelative)
import "Glob" System.FilePath.Glob (glob)
import Task (Task, getArgs, getConfig, toTask)

find :: Task [FilePath]
find = do
  Config {entry_points} <- Task.getConfig
  Args {entryPointGlob} <- Task.getArgs
  paths <-
    findFilesIn entry_points $ M.fromMaybe ("**" </> "*.*") entryPointGlob
  cwd <- toTask Dir.getCurrentDirectory
  case paths of
    [] -> throwError [NoModulesPresent $ show entry_points]
    _ -> return $ (makeRelative $ cwd </> entry_points) <$> paths

{-| Returns a list of files in the given direcory and all it's subdirectories.
-}
findFilesIn :: FilePath -> FilePath -> Task [FilePath]
findFilesIn path userGlob = do
  let globi = path </> userGlob
  toTask $ glob globi
