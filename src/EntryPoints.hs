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
import System.FilePath
       ((</>), makeRelative, normalise, takeDirectory)
import "Glob" System.FilePath.Glob (glob)
import Task (Task, lift)

find :: Args -> Config -> Task [FilePath]
find args config = do
  let entryPointsGlob = normalisedEntryPointsGlob config args
  paths <- findFilesIn entryPointsGlob
  cwd <- lift Dir.getCurrentDirectory
  case paths of
    [] -> throwError [NoModulesPresent (takeDirectory <$> entryPointsGlob)]
    _ -> return $ makeRelative (cwd </> entry_points config) <$> paths

normalisedEntryPointsGlob :: Config -> Args -> [FilePath]
normalisedEntryPointsGlob config args =
  case entryPointGlob args of
    [] -> [entry_points config </> "**" </> "*.*"]
    entryPoints
        -- handle arguments with and without a leading "./"
     -> (\entry -> "." </> normalise entry) <$> entryPoints

findFilesIn :: [FilePath] -> Task [FilePath]
findFilesIn paths = concat <$> traverse (lift . glob) paths
