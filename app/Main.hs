module Main where

import qualified Builder
import CliArguments (Args(..), readArguments)
import Config (Config(..))
import qualified Config
import qualified Message
import qualified Version
import qualified Watcher

main :: IO ()
main
  -- SETUP
 = do
  config@Config {version} <- Config.readConfig
  case Version.check version of
    Left err -> do
      print err
      run config
    Right _ -> run config

run :: Config -> IO ()
run config = do
  args@Args {version, watch} <- readArguments
  if version
    then Message.info Version.print
    else if watch
           then Watcher.watch config args
           else Builder.build config args
