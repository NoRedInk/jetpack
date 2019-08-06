module Main where

import Protolude
import qualified Builder
import qualified Cleaner
import CliArguments (Args(..), RunMode(..), readArguments)
import qualified Config
import Control.Monad (void, when)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified HotReload.Server
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
    Watch -> Watcher.watch config args Builder.DontHotReload
    HotReloading -> HotReload.Server.start config args Builder.HotReload
    RunOnce -> void $ Builder.build config args Builder.DontHotReload
