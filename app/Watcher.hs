{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Concurrent
import Twitch (defaultMainWithOptions, (|>), Options(..), LoggerType(..), DebounceType(..))
import qualified System.Console.AsciiProgress as Progress

main :: IO ()
main = do
  threadId <- forkIO Lib.run
  mVar <- newMVar threadId

  defaultMainWithOptions
     (Options
         NoLogger -- log
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
    Just childId -> killThread childId

  -- _ <- System.Process.system "reset"
  Progress.displayConsoleRegions $ do
    threadId <- forkIO Lib.run
    putMVar mVar threadId
