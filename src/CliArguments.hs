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
  <$> option auto
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
