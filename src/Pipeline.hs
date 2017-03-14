{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline
  ( Args(..)
  , Pipeline
  , PipelineF(..)
  , readCliArgs
  , readConfig
  , dependencies
  , noArgs
  , compile
  ) where

import Config (Config)
import Control.Monad.Free (Free, liftF)
import Data.Text as T
import Dependencies (Dependencies, Dependency)
import System.FilePath ()

-- TODO move this to args parser
data Args = Args
  { dry        :: Bool
  , verbose    :: Bool
  , configPath :: Maybe FilePath
  }

noArgs :: Args
noArgs = Args False False Nothing

data PipelineF a
  = ReadCliArgs (Args -> a)
  | ReadConfig (Maybe FilePath) (Config -> a)
  | Dependencies Config (Dependencies -> a)
  | Compile Config [Dependency] a
  deriving (Functor)

type Pipeline = Free PipelineF

readCliArgs :: Pipeline Args
readCliArgs = liftF $ ReadCliArgs id

readConfig :: Maybe FilePath -> Pipeline Config
readConfig maybePath = liftF $ ReadConfig maybePath id

dependencies :: Config -> Pipeline Dependencies
dependencies config = liftF $ Dependencies config id

compile :: Config -> [Dependency] -> Pipeline ()
compile config deps = liftF $ Compile config deps ()
