{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PackageJson where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (left)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Error (Error(..))
import GHC.Generics (Generic)
import Task (Task)

data PackageJson = PackageJson
  { main :: T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON PackageJson

{-| Loads a package.json
-}
load :: FilePath -> Task PackageJson
load path = do
  content <- lift $ BL.readFile path
  case Aeson.decode content of
    Just json -> return json
    Nothing -> left $ [JsonInvalid path]
