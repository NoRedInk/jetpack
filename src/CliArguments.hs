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
  , optimize :: Bool
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
  warn <-
    switch (long "warn" <> short 'w' <> help "Output compilation warnings.")
  optimize <-
    switch (long "optimize" <> short 'O' <> help "Compile Elm in optimized mode.")
  version <-
    switch (long "version" <> short 'v' <> help "display the version of jetpack.")
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
    , optimize =
        if optimize
          then True
          else optimize
    , time = time
    , clean = clean
    , runMode =
        if version
          then Version
          else if watch
                 then Watch
                 else RunOnce
    }
