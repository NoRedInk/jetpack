{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config(..)
  , load
  , defaultConfig
  ) where

import Control.Monad.Except
import Control.Monad.Trans.Class (lift)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Error (Error (..))
import GHC.Generics (Generic)
import System.FilePath ((</>))
import Task (Task)
import Utils.Files (fileExistsTask)

data Config = Config
  { module_directory     :: FilePath
  , source_directory     :: FilePath
  , elm_root_directory   :: FilePath
  , sass_load_paths      :: [FilePath]
  , temp_directory       :: FilePath
  , log_directory        :: FilePath
  , output_js_directory  :: FilePath
  , output_css_directory :: FilePath
  , elm_make_path        :: Maybe FilePath
  , sassc_path           :: Maybe FilePath
  , coffee_path          :: Maybe FilePath
  } deriving (Show, Eq, Generic)

defaultConfig :: Config
defaultConfig =
  Config
    { module_directory = "." </> "app" </> "assets" </> "modules"
    , source_directory = "." </> "ui" </> "src"
    , elm_root_directory = "." </> "ui"
    , sass_load_paths = sassLoadPaths
    , temp_directory = "." </> ".jetpack" </> "build_artifacts"
    , log_directory = "." </> ".jetpack" </> "logs"
      -- ("." </> "app" </> "assets" </> "javascripts" </> "webpack")
    , output_js_directory = "." </> ".jetpack" </> "js"
    , output_css_directory = "." </> "app" </> "assets" </> "stylesheets" </> "webpack"
    , elm_make_path = Just ("." </> "ui" </> "node_modules" </> ".bin" </> "elm-make")
    , sassc_path = Nothing
    , coffee_path = Just ("." </> "node_modules" </> ".bin" </> "coffee")
    }
  where
    sassLoadPaths =
      [ "node_modules"
      , "vendor" </> "assets" </> "components" </> "animatewithsass"
      , "app" </> "assets" </> "modules" </> "css"
      , "app" </> "assets" </> "stylesheets" </> "webpack"
      , "ui" </> "src"
      , "node_modules" </> "bourbon" </> "app" </> "assets" </> "stylesheets"
      , "node_modules" </> "bourbon-neat" </> "app" </> "assets" </> "stylesheets"
      ]

instance ToJSON Config

instance FromJSON Config

{-| Loads configuration for jetpack from `jetpack.json`.
-}
load :: FilePath -> Task Config
load root = do
  let path = root </> "jetpack.json"
  _ <- fileExistsTask path
  content <- lift $ BL.readFile path
  case Aeson.decode content of
    Just config -> lift $ return config
    Nothing     -> throwError $ [JsonInvalid path]
