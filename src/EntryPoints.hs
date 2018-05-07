{-# LANGUAGE DeriveAnyClass #-}

{-| Finds all entrypoints. Find either uses a passed glob or **/*.* to search in the `entry_points`.
-}
module EntryPoints
  ( find
  ) where

import CliArguments (Args(..))
import Config
import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import System.FilePath
       ((</>), makeRelative, normalise, takeDirectory)
import "Glob" System.FilePath.Glob (glob)

find :: Args -> Config -> IO [FilePath]
find args config = do
  let entryPointsGlob = normalisedEntryPointsGlob config args
  paths <- findFilesIn entryPointsGlob
  case paths of
    [] -> ES.throwM $ NoModulesPresent (takeDirectory <$> entryPointsGlob)
    _ -> return $ makeRelative (entryPoints config) <$> paths

normalisedEntryPointsGlob :: Config -> Args -> [FilePath]
normalisedEntryPointsGlob config args =
  case entryPointGlob args of
    [] -> [entryPoints config </> "**" </> "*.*"]
    entryPoints
        -- handle arguments with and without a leading "./"
     -> (\entry -> "." </> normalise entry) <$> entryPoints

findFilesIn :: [FilePath] -> IO [FilePath]
findFilesIn paths = concat <$> traverse glob paths

data Error =
  NoModulesPresent [FilePath]
  deriving (Typeable, Exception)

instance Show Error where
  show (NoModulesPresent paths) =
    T.unpack $
    T.unlines
      [ "It seems to me that you either provided a wrong `entry_points` or you don't have any modules."
      , ""
      , "I didn't find anything in " <> T.pack (show paths)
      , ""
      ]
