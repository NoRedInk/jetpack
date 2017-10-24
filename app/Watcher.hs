{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Config
import Control.Concurrent
import qualified Lib
import qualified System.Console.AsciiProgress as Progress
import qualified System.Directory as Dir
import qualified Task
import Twitch
    ( DebounceType (..)
    , LoggerType (..)
    , Options (..)
    , defaultMainWithOptions
    , (|>)
    )

main :: IO ()
main = do
  threadId <- forkIO Lib.run
  mVar <- newMVar threadId

  cwd <- Dir.getCurrentDirectory
  maybeConfig <- Task.runTask $ Config.load cwd
  case maybeConfig of
    Left _ -> putStrLn "no jetpack config found."
    Right config ->
      defaultMainWithOptions
        (Options
            NoLogger -- log
            Nothing -- logFile
            (Config.source_directory <$> config) -- root
            True -- recurseThroughDirectories
            Twitch.Debounce -- debounce
            1 -- debounceAmount
            0 -- pollInterval
            False -- usePolling
        ) $ do
          "**/*.elm" |> \_ -> rebuild mVar
          "**/*.coffee" |> \_ -> rebuild mVar
          "**/*.js" |> \_ -> rebuild mVar
          "**/*.sass" |> \_ -> rebuild mVar
          "**/*.scss" |> \_ -> rebuild mVar
          "**/*.json" |> \_ -> rebuild mVar

rebuild :: MVar Control.Concurrent.ThreadId -> IO ()
rebuild mVar = do
  childId  <- tryTakeMVar mVar
  traverse killThread childId

  -- _ <- System.Process.system "reset"
  Progress.displayConsoleRegions $ do
    threadId <- forkIO Lib.run
    putMVar mVar threadId
