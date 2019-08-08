{-# LANGUAGE DeriveAnyClass #-}

{-| Finds all entrypoints. Find either uses a passed glob or **/*.* to search in the `entry_points`.
-}
module EntryPoints
  ( find
  )
where

import CliArguments (Args (..))
import qualified Config
import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import System.FilePath
  ( (</>)
  , makeRelative
  , normalise
  , takeDirectory
  )
import "Glob" System.FilePath.Glob (glob)

find :: Args -> Config.EntryPoints -> IO [FilePath]
find args entryPoints = do
  let entryPointsGlob = normalisedEntryPointsGlob entryPoints args
  paths <- findFilesIn entryPointsGlob
  case paths of
    [] -> ES.throwM $ NoModulesPresent (takeDirectory <$> entryPointsGlob)
    _ -> return $ makeRelative (Config.unEntryPoints entryPoints) <$> paths

normalisedEntryPointsGlob :: Config.EntryPoints -> Args -> [FilePath]
normalisedEntryPointsGlob entryPoints args =
  case entryPointGlob args of
    [] -> [Config.unEntryPoints entryPoints </> "**" </> "*.*"]
    entryPoints ->
      -- handle arguments with and without a leading "./"
      (\entry -> "." </> normalise entry) <$> entryPoints

findFilesIn :: [FilePath] -> IO [FilePath]
findFilesIn paths = concat <$> traverse glob paths

data Error
  = NoModulesPresent [FilePath]
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
