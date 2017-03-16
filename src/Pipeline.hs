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
  , setup
  , concatModules
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

data PipelineF next
  = ReadCliArgs (Args -> next)
  | ReadConfig (Maybe FilePath) (Config -> next)
  | Dependencies Config (Dependencies -> next)
  | Compile Config [Dependency] next
  | Init Config next
  | ConcatModules Config Dependencies next
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

setup :: Config -> Pipeline ()
setup config = liftF $ Init config ()

concatModules :: Config -> Dependencies -> Pipeline ()
concatModules config dependencies = liftF $ ConcatModules config dependencies ()
