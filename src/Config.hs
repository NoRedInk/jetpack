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
    { entry_points = "." </> "modules"
    , modules_directories = [ "." </> "node_modules" ]
    , source_directory = "." </> "src"
    , elm_root_directory = "."
    , sass_load_paths = sassLoadPaths
    , temp_directory = "." </> ".jetpack" </> "build_artifacts"
    , log_directory = "." </> ".jetpack" </> "logs"
    , output_js_directory = "." </> "dist" </> "javascripts" </> "jetpack"
    , output_css_directory = "." </> "dist" </> "stylesheets" </> "jetpack"
    , elm_make_path = Just ("." </> "node_modules" </> ".bin" </> "elm-make")
    , sassc_path = Just ("." </> "bin" </> "sass")
    , coffee_path = Just ("." </> "node_modules" </> ".bin" </> "coffee")
    , no_parse = []
    }
  where
    sassLoadPaths =
      [ "node_modules"
      , "src"
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
