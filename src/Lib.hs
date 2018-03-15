module Lib
  ( run
  ) where

import qualified Builder
import CliArguments (Args(..), readArguments)
import qualified Config
import qualified Message
import qualified Version
import qualified Watcher

run :: IO ()
run
  -- SETUP
 = do
  config <- Config.readConfig
  args@Args {version, watch} <- readArguments
  if version
    then Message.info Version.print
    else if watch
           then Watcher.watch config args
           else Builder.build config args
