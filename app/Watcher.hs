{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Config
import Control.Concurrent
import Data.Foldable (for_)
import qualified Lib
import qualified System.Console.AsciiProgress as Progress
import qualified System.Directory as Dir
import qualified Task
import Twitch
    ( DebounceType (..)
    , Dep
    , LoggerType (..)
    , Options (..)
    , addModify
    , defaultMainWithOptions
    )

main :: IO ()
main = Progress.displayConsoleRegions $ do
  cwd <- Dir.getCurrentDirectory
  maybeConfig <- Task.runTask $ Config.load cwd
  case maybeConfig of
    Right (Just config) -> watch config
    _                   -> putStrLn "no jetpack config found."

watch :: Config.Config -> IO ()
watch config = do
  mVar <- newEmptyMVar
  defaultMainWithOptions (options config)
    $ for_ fileTypesToWatch
    $ addModify
    $ const
    $ rebuild mVar

fileTypesToWatch :: [Dep]
fileTypesToWatch =
  [ "**/*.elm"
  , "**/*.coffee"
  , "**/*.js"
  , "**/*.sass"
  , "**/*.scss"
  , "**/*.json"
  ]

options :: Config.Config -> Options
options config =
  Options
    NoLogger -- log
    Nothing -- logFile
    (Just $ Config.source_directory config) -- root
    True -- recurseThroughDirectories
    Twitch.Debounce -- debounce
    1 -- debounceAmount
    0 -- pollInterval
    False -- usePolling

rebuild :: MVar Control.Concurrent.ThreadId -> IO ()
rebuild mVar = do
  childId  <- tryTakeMVar mVar
  for_ childId killThread
  threadId <- forkIO Lib.run
  putMVar mVar threadId
