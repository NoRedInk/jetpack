module Config
  ( Config(..)
  , readConfig
  , load
  , defaultConfig
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Error (Error(..))
import qualified Error
import GHC.Generics (Generic)
import Message
import qualified System.Directory as Dir
import System.Exit
import System.FilePath ((</>))

data Config = Config
  { entry_points :: FilePath
  , modules_directories :: [FilePath]
  , source_directory :: FilePath
  , elm_root_directory :: FilePath
  , temp_directory :: FilePath
  , log_directory :: FilePath
  , output_js_directory :: FilePath
  , elm_make_path :: Maybe FilePath
  , coffee_path :: Maybe FilePath
  , no_parse :: [FilePath]
  , watch_file_extensions :: [T.Text]
  , watch_file_ignore_patterns :: [T.Text]
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON Config

readConfig :: IO Config
readConfig = do
  cwd <- Dir.getCurrentDirectory
  maybeLocalConfig <- load cwd
  case maybeLocalConfig of
    Just config -> return config
    Nothing -> return defaultConfig

defaultConfig :: Config
defaultConfig =
  Config
  { entry_points = "." </> "modules"
  , modules_directories = ["." </> "node_modules"]
  , source_directory = "." </> "src"
  , elm_root_directory = "."
  , temp_directory = "." </> ".jetpack" </> "build_artifacts"
  , log_directory = "." </> ".jetpack" </> "logs"
  , output_js_directory = "." </> "dist" </> "javascripts" </> "jetpack"
  , elm_make_path = Just ("." </> "node_modules" </> ".bin" </> "elm-make")
  , coffee_path = Just ("." </> "node_modules" </> ".bin" </> "coffee")
  , no_parse = []
  , watch_file_extensions = [".elm", ".coffee", ".js", ".json"]
  -- Ignore files like Emacs' backup files (`.#filename`) and other backup files
  -- (`~filename`). Files like these are often created when editing begins, as a
  -- recovery file for example, and do not imply that a build should be done.
  , watch_file_ignore_patterns = ["/[.]#[^/]*$", "/~[^/]*$"]
  }

{-| Loads configuration for jetpack from `jetpack.json`.
-}
load :: FilePath -> IO (Maybe Config)
load root = do
  let path = root </> "jetpack.json"
  exists <- Dir.doesFileExist path
  if exists
    then do
      content <- BL.readFile path
      case Aeson.eitherDecode content of
        Right config -> return $ Just config
        Left err -> do
          _ <-
            Message.error $ Error.description $ ConfigInvalid path $ T.pack err
          System.Exit.exitFailure
    else return Nothing
