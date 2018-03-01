module Config
  ( Config(..)
  , readConfig
  , load
  , defaultConfig
  ) where

import Control.Monad.Except
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Error (Error(..))
import GHC.Generics (Generic)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Task (Task, lift)

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
  } deriving (Show, Eq, Generic)

instance ToJSON Config

instance FromJSON Config

readConfig :: Task Config
readConfig = do
  cwd <- lift Dir.getCurrentDirectory
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
  }

{-| Loads configuration for jetpack from `jetpack.json`.
-}
load :: FilePath -> Task (Maybe Config)
load root = do
  let path = root </> "jetpack.json"
  exists <- lift $ Dir.doesFileExist path
  if exists
    then do
      content <- lift $ BL.readFile path
      case Aeson.decode content of
        Just config -> return $ Just config
        Nothing -> throwError [JsonInvalid path]
    else return Nothing
