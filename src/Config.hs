module Config
  ( Config(..)
  , readConfig
  , load
  , defaultConfig
  ) where

import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Error (Error(..))
import GHC.Generics (Generic)
import Message
import qualified System.Directory as Dir
import System.Exit
import System.FilePath ((</>))
import qualified Version

data Config = Config
  { version :: T.Text
  , entry_points :: FilePath
  , modules_directories :: [FilePath]
  , source_directory :: FilePath
  , elm_root_directory :: FilePath
  , temp_directory :: FilePath
  , log_directory :: FilePath
  , output_js_directory :: FilePath
  , elm_make_path :: Maybe FilePath
  , coffee_path :: Maybe FilePath
  , no_parse :: [FilePath]
  } deriving (Show, Eq, Generic)

instance ToJSON Config

instance FromJSON Config

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
  { version = Version.print
  , entry_points = "." </> "modules"
  , modules_directories = ["." </> "node_modules"]
  , source_directory = "." </> "src"
  , elm_root_directory = "."
  , temp_directory = "." </> ".jetpack" </> "build_artifacts"
  , log_directory = "." </> ".jetpack" </> "logs"
  , output_js_directory = "." </> "dist" </> "javascripts" </> "jetpack"
  , elm_make_path = Just ("." </> "node_modules" </> ".bin" </> "elm-make")
  , coffee_path = Just ("." </> "node_modules" </> ".bin" </> "coffee")
  , no_parse = []
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
          Message.error [ConfigInvalid path err]
          System.Exit.exitFailure
    else return Nothing
