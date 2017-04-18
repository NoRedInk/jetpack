{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
module Env
  ( Env(..)
  , Args(..)
  , Config(..)
  ) where

import Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified System.Console.AsciiProgress as Progress

data Env = Env
  { progressBar :: Maybe Progress.ProgressBar
  , config      :: Config
  , args        :: Args
  }

data Args = Args
  { entryPointGlob :: Maybe String
  , configPath     :: Maybe FilePath
  , debug          :: Bool
  }

data Config = Config
  { entry_points         :: FilePath
  , modules_directories  :: [FilePath]
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


instance ToJSON Config

instance FromJSON Config
