module Config
  ( Config(..)
  , readConfig
  , load
  , defaultConfig
  , module Env
  ) where

import Control.Monad.Except
import Control.Monad.State (modify)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Env
import Error (Error (..))
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Task (Task, toTask)


readConfig :: Task Config
readConfig = do
  cwd <- toTask Dir.getCurrentDirectory
  maybeLocalConfig <- load cwd
  c <- case maybeLocalConfig of
         Just config -> return config
         Nothing     -> return defaultConfig
  modify (\env -> env { config = c })
  return c

defaultConfig :: Config
defaultConfig =
  Config
    { entry_points = "." </> "app" </> "assets" </> "modules"
    , source_directory = "." </> "ui" </> "src"
    , elm_root_directory = "." </> "ui"
    , sass_load_paths = sassLoadPaths
    , temp_directory = "." </> ".jetpack" </> "build_artifacts"
    , log_directory = "." </> ".jetpack" </> "logs"
    , output_js_directory = "." </> "app" </> "assets" </> "javascripts" </> "webpack"
    , output_css_directory = "." </> "app" </> "assets" </> "stylesheets" </> "webpack"
    , elm_make_path = Just ("." </> "ui" </> "node_modules" </> ".bin" </> "elm-make")
    , sassc_path = Just ("." </> "ui" </> "bin" </> "sass")
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
{-| Loads configuration for jetpack from `jetpack.json`.
-}
load :: FilePath -> Task (Maybe Config)
load root = do
  let path = root </> "jetpack.json"
  exists <- toTask $ Dir.doesFileExist path
  if exists
    then do
      content <- toTask $ BL.readFile path
      case Aeson.decode content of
        Just config -> return $ Just config
        Nothing     -> throwError $ [JsonInvalid path]
    else return Nothing
