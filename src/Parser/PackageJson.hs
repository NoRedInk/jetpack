module Parser.PackageJson where

import Control.Monad.Except (throwError)

import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Error (Error(..))
import GHC.Generics (Generic)
import Task (Task, lift)
import Utils.Files (fileExistsTask)

data PackageJson = PackageJson
  { main :: Maybe T.Text
  , browser :: Maybe T.Text
  } deriving (Show, Eq, Generic)

instance FromJSON PackageJson

{-| Loads a package.json
-}
load :: FilePath -> Task PackageJson
load path = do
  _ <- fileExistsTask path
  content <- lift $ BL.readFile path
  case Aeson.eitherDecode content of
    Left err -> throwError [JsonInvalid path (T.pack err)]
    Right json -> return json
