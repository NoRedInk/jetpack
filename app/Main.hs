module Main where

import qualified Builder
import qualified Cleaner
import CliArguments (Args(..), RunMode(..), readArguments)
import Config (Config(..))
import qualified Config
import Control.Monad (when)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Error
import qualified Message
import qualified Parser.JetpackVersion as JetpackVersion
import qualified Version
import qualified Watcher

main :: IO ()
main
  -- SETUP
 = do
  maybeVersion <- JetpackVersion.load
  case Version.check maybeVersion of
    Just err -> Message.warning err
    Nothing -> return ()
  config <- Config.readConfig
  args@Args {clean, runMode} <- readArguments
  when clean (Cleaner.clean config)
  case runMode of
    Version -> TIO.putStrLn Version.print
    Watch -> Watcher.watch config args
    RunOnce -> Builder.build config args
