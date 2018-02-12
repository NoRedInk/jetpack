module CliArguments
  ( Args(..)
  , defaultArguments
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
  , version :: Bool
  , time :: Bool
  , watch :: Bool
  }

defaultArguments :: Args
defaultArguments =
  Args
  { entryPointGlob = []
  , configPath = Nothing
  , debug = False
  , warn = False
  , postHook = Nothing
  , preHook = Nothing
  , version = False
  , time = False
  , watch = False
  }

readArguments :: IO Args
readArguments =
  execParser $ info (parser <**> helper) $ fullDesc <> progDesc "🚀 📦"

parser :: Parser Args
parser =
  Args <$> many (strArgument (help "Entry points to compile.")) <*>
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
  switch (long "time" <> short 't' <> help "display compile times.") <*>
  switch (long "watch" <> short 'w' <> help "watch for changes.")
  where
    go :: String -> Maybe (Maybe String)
    go "" = Just Nothing
    go str = Just $ Just str
