{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
module Config (Config(..), load) where

{-| Configuration of jetpack.
-}

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Data.Aeson as Aeson
import Errors (Error(..))
import FileUtils (fileExists)
import GHC.Generics (Generic)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BL


data Config = Config
  { module_directory :: FilePath
  , source_directory :: FilePath
  , temp_directory :: FilePath
  , output_js_directory :: FilePath
  , output_css_directory :: FilePath
  } deriving (Show, Generic)


instance ToJSON Config
instance FromJSON Config


{-| Loads configuration for jetpack from `jetpack.json`.
-}
load :: FilePath -> EitherT Error IO Config
load root = do
  let path = root </> "jetpack.json"
  _ <- fileExists path
  content <- lift $ BL.readFile path
  case Aeson.decode content of
    Just config -> lift $ return config
    Nothing -> left $ JsonInvalid path

