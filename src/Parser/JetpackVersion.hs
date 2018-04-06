module Parser.JetpackVersion where

import Control.Monad.Except (throwError)

import Control.Monad (fail)
import Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Data.SemVer as SemVer
import qualified Data.Text as T
import Error (Error(..))
import GHC.Generics (Generic)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import Task (Task, lift)
import Utils.Files (fileExistsTask)

data Version = Version
  { version :: SemVer.Version
  } deriving (Show, Eq, Generic)

instance FromJSON Version where
  parseJSON =
    withObject "devDependencies" $ \v ->
      Version <$>
      (v .: "devDependencies" >>= (.: "@noredink/jetpack") >>= toSemVer)

toSemVer :: T.Text -> Parser SemVer.Version
toSemVer v =
  case SemVer.fromText v of
    Left err -> fail err
    Right semVer -> return semVer

{-| Loads a package.json
-}
load :: Task Version
load = do
  cwd <- lift Dir.getCurrentDirectory
  let path = cwd </> "package.json"
  _ <- fileExistsTask path
  content <- lift $ BL.readFile path
  case Aeson.eitherDecode content of
    Left err -> throwError [JsonInvalid path (T.pack err)]
    Right json -> return json
