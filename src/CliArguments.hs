module CliArguments
  ( Args (..)
  , defaultArguments
  , readArguments
  , module Env
  ) where

import Control.Monad.State (modify)
import Data.Semigroup ((<>))
import Env
import Options.Applicative
import System.FilePath ()
import Task

defaultArguments :: Args
defaultArguments = Args
  { entryPointGlob = Nothing
  , configPath = Nothing
  , debug = False
  , warn = False
  , postHook = Nothing
  , preHook = Nothing
  }

readArguments :: Task Args
readArguments = do
  a <- toTask $ execParser
    $ info (parser <**> helper)
    $ fullDesc <> progDesc "🚀 📦"
  _ <- modify (\env -> env { args = a })
  return a

parser :: Parser Args
parser = Args
  <$> option (maybeReader go)
      ( long "entry"
      <> short 'e'
      <> value Nothing
      <> help "Glob entry points." )
  <*> option auto
      ( long "config"
      <> short 'c'
      <> value Nothing
      <> help "Path to config file." )
  <*> switch
      ( long "debug"
      <> short 'd'
      <> help "Run jetpack in debug mode." )
  <*> switch
      ( long "warn"
      <> short 'w'
      <> help "Output elm make warnings." )
  <*> option (maybeReader go)
      ( long "pre-hook"
      <> value Nothing
      <> help "Bash commands that will get executed before jetpack runs." )
  <*> option (maybeReader go)
      ( long "post-hook"
      <> value Nothing
      <> help "Bash commands that will get executed after jetpack runs." )
  where
    go :: String -> Maybe (Maybe String)
    go ""  = Just Nothing
    go str = Just $ Just str
