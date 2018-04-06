module Main where

import qualified Builder
import qualified Cleaner
import CliArguments (Args(..), RunMode(..), readArguments)
import Config (Config(..))
import qualified Config
import Control.Monad (when)
import qualified Message
import qualified Parser.JetpackVersion as JetpackVersion
import qualified Task
import qualified Version
import qualified Watcher

main :: IO ()
main
  -- SETUP
 = do
  maybeVersion <- Task.runExceptT JetpackVersion.load
  case maybeVersion of
    Left err -> Message.error err
    Right jetpackVersion -> do
      _ <-
        case Version.check jetpackVersion of
          Just err -> Message.warningHeader err
          Nothing -> return ()
      config <- Config.readConfig
      args@Args {clean, runMode} <- readArguments
      when clean (Cleaner.clean config)
      case runMode of
        Version -> Message.info Version.print
        Watch -> Watcher.watch config args
        RunOnce -> Builder.build config args
