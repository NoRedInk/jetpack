module Config
  ( Config(..)
  , readConfig
  , load
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Message
import qualified System.Directory as Dir
import System.Exit
import System.FilePath ((</>))

data Config = Config
  { entryPoints :: FilePath
  , modulesDirs :: [FilePath]
  , sourceDir :: FilePath
  , elmRoot :: FilePath
  , tempDir :: FilePath
  , logDir :: FilePath
  , outputDir :: FilePath
  , elmMakePath :: Maybe FilePath
  , coffeePath :: Maybe FilePath
  , noParse :: [FilePath]
  , watchFileExt :: [T.Text]
  , watchIgnorePatterns :: [T.Text]
  , replayScriptPath :: Maybe FilePath
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
    v .:? "watch_file_ignore_patterns" .!= ["/[.]#[^/]*$", "/~[^/]*$"] <*>
    v .:? "replay_script_path"
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
            Message.error $
            T.unlines
              [ "Invalid jetpack.json: " <> T.pack path
              , ""
              , "    " <> T.pack err
              , ""
              ]
          System.Exit.exitFailure
    else do
      _ <-
        Message.error $
        T.unlines ["I didn't find a config for jetpack at " <> T.pack path]
      System.Exit.exitFailure
