{-# LANGUAGE ApplicativeDo #-}

module CliArguments
  ( Args(..)
  , RunMode(..)
  , readArguments
  ) where

import Data.Semigroup ((<>))
import Options.Applicative
import System.FilePath ()

data Args = Args
  { entryPointGlob :: [String]
  , configPath :: Maybe FilePath
  , debug :: Bool
  , warn :: Bool
  , preHook :: Maybe String
  , postHook :: Maybe String
  , time :: Bool
  , clean :: Bool
  , runMode :: RunMode
  }

data RunMode
  = RunOnce
  | Watch
  | Version

readArguments :: IO Args
readArguments =
  execParser (info (parser <**> helper) $ fullDesc <> progDesc "🚀 📦")

parser :: Parser Args
parser = do
  entryPointGlob <- many (strArgument (help "Entry points to compile."))
  configPath <-
    option
      auto
      (long "config" <> short 'c' <> value Nothing <>
       help "Path to config file.")
  debug <-
    switch (long "debug" <> short 'd' <> help "Run jetpack in debug mode.")
  warn <- switch (long "warn" <> short 'w' <> help "Output elm make warnings.")
  preHook <-
    option
      (maybeReader go)
      (long "pre-hook" <> value Nothing <>
       help "Bash commands that will get executed before jetpack runs.")
  postHook <-
    option
      (maybeReader go)
      (long "post-hook" <> value Nothing <>
       help "Bash commands that will get executed after jetpack runs.")
  version <-
    switch
      (long "version" <> short 'v' <> help "display the version of jetpack")
  time <- switch (long "time" <> short 't' <> help "display compile times.")
  watch <- switch (long "watch" <> short 'w' <> help "watch for changes.")
  clean <-
    switch
      (long "clean" <> short 'c' <> help "Cleans elm-stuff and removes .jetpack")
  return
    Args
    { entryPointGlob = entryPointGlob
    , configPath = configPath
    , debug =
        if watch
          then True
          else debug
    , warn =
        if watch
          then True
          else warn
    , postHook = postHook
    , preHook = preHook
    , time = time
    , clean = clean
    , runMode =
        if version
          then Version
          else if watch
                 then Watch
                 else RunOnce
    }
  where
    go :: String -> Maybe (Maybe String)
    go "" = Just Nothing
    go str = Just $ Just str
