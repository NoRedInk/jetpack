{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Concurrent
import Twitch (defaultMainWithOptions, (|>), Options(..), LoggerType(..), DebounceType(..))

main :: IO ()
main =
  defaultMainWithOptions
     (Options
         Twitch.LogToStdout -- log
         -- Twitch.NoLogger -- log
         Nothing -- logFile
         (Just "ui/src") -- root
         True -- recurseThroughDirectories
         Twitch.Debounce -- debounce
         1 -- debounceAmount
         0 -- pollInterval
         False -- usePolling
     )
  $ do
  "**/*.elm" |>
    \_ -> Control.Concurrent.forkIO Lib.run
