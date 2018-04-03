module Main where

import qualified Builder
import CliArguments (Args(..), readArguments)
import Config (Config(..))
import qualified Config
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
      args@Args {version, watch} <- readArguments
      if version
        then Message.info Version.print
        else if watch
               then Watcher.watch config args
               else Builder.build config args
