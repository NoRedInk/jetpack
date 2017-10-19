{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Twitch (defaultMainWithOptions, (|>), Options(..), LoggerType(..), DebounceType(..))

main :: IO ()
main =
  defaultMainWithOptions
     (Options
         NoLogger
         Nothing
         (Just "ui/src")
         True
         Debounce
         500
         0
         False)
  $ do
  "*.elm"   |> \_ -> Lib.run
