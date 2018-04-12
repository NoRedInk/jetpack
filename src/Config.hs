module Config
  ( Config(..)
  , readConfig
  , load
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Error (Error(..))
import qualified Error
import Message
import qualified System.Directory as Dir
import System.Exit
import System.FilePath ((</>))

data Config = Config
  { entry_points :: FilePath
  , modules_directories :: [FilePath]
  , source_directory :: FilePath
  , elm_root_directory :: FilePath
  , temp_directory :: FilePath
  , log_directory :: FilePath
  , output_js_directory :: FilePath
  , elm_make_path :: Maybe FilePath
  , coffee_path :: Maybe FilePath
  , no_parse :: [FilePath]
  , watch_file_extensions :: [T.Text]
  , watch_file_ignore_patterns :: [T.Text]
  } deriving (Show, Eq)

instance Aeson.FromJSON Config where
  parseJSON (Aeson.Object v) =
    Config --
     <$>
    v .: "entry_points" <*>
    v .: "modules_directories" <*>
    v .: "source_directory" <*>
    v .: "elm_root_directory" <*>
    v .: "temp_directory" .!= "./.jetpack/build_artifacts" <*>
    v .:? "log_directory" .!= "./.jetpack/logs" <*>
    v .: "output_js_directory" <*>
    v .:? "elm_make_path" <*>
    v .:? "coffee_path" <*>
    v .:? "no_parse" .!= [] <*>
    v .:? "watch_file_extensions" .!= [".elm", ".coffee", ".js", ".json"] <*>
    v .:? "watch_file_ignore_patterns" .!= ["/[.]#[^/]*$", "/~[^/]*$"]
  parseJSON invalid = typeMismatch "Config" invalid

readConfig :: IO Config
readConfig = do
  cwd <- Dir.getCurrentDirectory
  load cwd

{-| Loads configuration for jetpack from `jetpack.json`.
-}
load :: FilePath -> IO (Config)
load root = do
  let path = root </> "jetpack.json"
  exists <- Dir.doesFileExist path
  if exists
    then do
      content <- BL.readFile path
      case Aeson.eitherDecode content of
        Right config -> return config
        Left err -> do
          _ <-
            Message.error $ Error.description $ ConfigInvalid path $ T.pack err
          System.Exit.exitFailure
    else do
      _ <- Message.error $ Error.description $ NoConfigFound path
      System.Exit.exitFailure
