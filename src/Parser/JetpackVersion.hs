{-# LANGUAGE DeriveAnyClass #-}

module Parser.JetpackVersion
  ( Version(..)
  , load
  ) where

import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import Control.Monad (fail)
import Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Data.SemVer as SemVer
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified System.Directory as Dir
import System.FilePath (FilePath, (</>), dropFileName)

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
load :: IO Version
load = do
  cwd <- Dir.getCurrentDirectory
  let path = cwd </> "package.json"
  content <- BL.readFile path
  case Aeson.eitherDecode content of
    Left err -> ES.throwM $ JsonInvalid path $ T.pack err
    Right json -> return json

data Error =
  JsonInvalid FilePath
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
