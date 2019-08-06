{-# LANGUAGE DeriveAnyClass #-}

module Parser.PackageJson
  ( load
  , PackageJson (..)
  )
where

import Protolude
import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.FilePath (FilePath, dropFileName)
import qualified Text.Show

data PackageJson
  = PackageJson
      { main :: Maybe FilePath
      , browser :: Maybe FilePath
      }
  deriving (Show, Eq, Generic)

instance FromJSON PackageJson

{-| Loads a package.json
-}
load :: FilePath -> IO PackageJson
load path = do
  content <- BL.readFile path
  case Aeson.eitherDecode content of
    Left err -> ES.throwM $ JsonInvalid path $ T.pack err
    Right json -> return json

data Error
  = JsonInvalid
      FilePath
      T.Text
  deriving (Typeable, Exception)

instance Show Error where

  show (JsonInvalid file err) =
    T.unpack $
      T.unlines
        [ "I couldn't decode package.json in " <> (T.pack $ dropFileName file)
        , ""
        , "    " <> err
        , ""
        ]
