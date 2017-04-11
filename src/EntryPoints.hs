{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

{-| Finds all entrypoints. Find either uses a passed glob or **/*.* to search in the `module_directory`.
-}

module EntryPoints (find) where

import CliArguments (Args (..))
import Config
import Control.Monad.Except (throwError)

import qualified Data.Maybe as M
import Error
import qualified System.Directory as Dir
import System.FilePath (makeRelative, (</>))
import "Glob" System.FilePath.Glob (glob)
import Task (Task, toTask)

find :: Config -> Args -> Task [FilePath]
find Config {module_directory} Args{entryPointGlob} = do
  paths <- findFilesIn module_directory $ M.fromMaybe ( "**" </> "*.*" ) entryPointGlob
  cwd <- toTask Dir.getCurrentDirectory
  case paths of
    [] -> throwError [NoModulesPresent $ show module_directory]
    _  -> return $ (makeRelative $ cwd </> module_directory) <$> paths

{-| Returns a list of files in the given direcory and all it's subdirectories.
-}
findFilesIn :: FilePath -> FilePath -> Task [FilePath]
findFilesIn path userGlob = do
  let globi = path </> userGlob
  toTask $ glob globi
