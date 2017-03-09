{-# OPTIONS_GHC -Wall #-}

module Utils.Files
  ( fileExistsTask
  , findAllFilesIn
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Error (Error(..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import Task (Task)

{-| Checks if file exists and returns a failing task if it doesn't
-}
fileExistsTask :: FilePath -> Task ()
fileExistsTask path = do
  exists <- lift $ doesFileExist path
  case exists of
    True -> lift $ return ()
    False -> left $ [FileNotFound (show path)]

{-| Returns a list of files in the given direcory and all it's subdirectories.
-}
findAllFilesIn :: FilePath -> Task [FilePath]
findAllFilesIn path = do
  let globi = path </> "**" </> "*.*"
  lift $ glob globi
