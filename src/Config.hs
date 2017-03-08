{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Config(..)
  , load
  , defaultConfig
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Error (Error(..))
import GHC.Generics (Generic)
import System.FilePath ((</>))
import Task (Task)
import Utils.Files (fileExists)

data Config = Config
  { module_directory :: FilePath
  , source_directory :: FilePath
  , temp_directory :: FilePath
  , output_js_directory :: FilePath
  , output_css_directory :: FilePath
  } deriving (Show, Eq, Generic)

defaultConfig :: Config
defaultConfig =
  Config
    ("." </> "app" </> "assets" </> "modules")
    ("." </> "ui" </> "src")
    ("." </> "tmp")
    ("." </> "app" </> "assets" </> "javascript" </> "webpack")
    ("." </> "app" </> "assets" </> "stylesheets" </> "webpack")

instance ToJSON Config

instance FromJSON Config

{-| Loads configuration for jetpack from `jetpack.json`.
-}
load :: FilePath -> Task Config
load root = do
  let path = root </> "jetpack.json"
  _ <- fileExists path
  content <- lift $ BL.readFile path
  case Aeson.decode content of
    Just config -> lift $ return config
    Nothing -> left $ JsonInvalid path
