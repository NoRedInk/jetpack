{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Concurrent
import Twitch (defaultMainWithOptions, (|>), Options(..), LoggerType(..), DebounceType(..))

main :: IO ()
main = do
  threadId <- Control.Concurrent.forkIO Lib.run
  mVar <- Control.Concurrent.newMVar threadId

  defaultMainWithOptions
     (Options
         Twitch.LogToStdout -- log
         Nothing -- logFile
         (Just "ui/src") -- root
         True -- recurseThroughDirectories
         Twitch.Debounce -- debounce
         1 -- debounceAmount
         0 -- pollInterval
         False -- usePolling
     )
     ("**/*.elm" |> \_ -> rebuild mVar)

rebuild :: MVar Control.Concurrent.ThreadId -> IO ()
rebuild mVar = do
  maybeChildId  <- tryTakeMVar mVar

  case maybeChildId of
    Nothing -> pure ()
    Just childId -> Control.Concurrent.killThread childId

  threadId <- Control.Concurrent.forkIO Lib.run
  Control.Concurrent.putMVar mVar threadId
