module CliArguments
  ( Args(..)
  , defaultArguments
  , readArguments
  ) where

import Data.Semigroup ((<>))
import Options.Applicative
import System.FilePath ()
import Task

data Args = Args
  { entryPointGlob :: Maybe String
  , configPath :: Maybe FilePath
  , debug :: Bool
  , warn :: Bool
  , preHook :: Maybe String
  , postHook :: Maybe String
  , version :: Bool
  , time :: Bool
  }

defaultArguments :: Args
defaultArguments =
  Args
  { entryPointGlob = Nothing
  , configPath = Nothing
  , debug = False
  , warn = False
  , postHook = Nothing
  , preHook = Nothing
  , version = False
  , time = False
  }

readArguments :: Task Args
readArguments =
  toTask $ execParser $ info (parser <**> helper) $ fullDesc <> progDesc "🚀 📦"

parser :: Parser Args
parser =
  Args <$>
  option
    (maybeReader go)
    (long "entry" <> short 'e' <> value Nothing <> help "Glob entry points.") <*>
  option
    auto
    (long "config" <> short 'c' <> value Nothing <> help "Path to config file.") <*>
  switch (long "debug" <> short 'd' <> help "Run jetpack in debug mode.") <*>
  switch (long "warn" <> short 'w' <> help "Output elm make warnings.") <*>
  option
    (maybeReader go)
    (long "pre-hook" <> value Nothing <>
     help "Bash commands that will get executed before jetpack runs.") <*>
  option
    (maybeReader go)
    (long "post-hook" <> value Nothing <>
     help "Bash commands that will get executed after jetpack runs.") <*>
  switch (long "version" <> short 'v' <> help "display the version of jetpack") <*>
  switch (long "time" <> short 't' <> help "display compile times.")
  where
    go :: String -> Maybe (Maybe String)
    go "" = Just Nothing
    go str = Just $ Just str
