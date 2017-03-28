{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline
  ( Pipeline
  , PipelineF(..)
  , readCliArgs
  , readConfig
  , dependencies
  , compile
  , setup
  , concatModules
  ) where

import Config (Config)
import Control.Monad.Free (Free, liftF)
import Dependencies (Dependencies, Dependency)
import System.FilePath ()
import ToolPaths
import CliArguments (Args)


data PipelineF next
  = ReadCliArgs (Args -> next)
  | ReadConfig (Maybe FilePath) (Config -> next)
  | Dependencies Config (Dependencies -> next)
  | Compile Config ToolPaths [Dependency] next
  | Init Config (ToolPaths -> next)
  | ConcatModules Config Dependencies ([FilePath] -> next)
  deriving (Functor)

type Pipeline = Free PipelineF

readCliArgs :: Pipeline Args
readCliArgs = liftF $ ReadCliArgs id

readConfig :: Maybe FilePath -> Pipeline Config
readConfig maybePath = liftF $ ReadConfig maybePath id

dependencies :: Config -> Pipeline Dependencies
dependencies config = liftF $ Dependencies config id

compile :: Config -> ToolPaths -> [Dependency] -> Pipeline ()
compile config toolPaths deps = liftF $ Compile config toolPaths deps ()

setup :: Config -> Pipeline ToolPaths
setup config = liftF $ Init config id

concatModules :: Config -> Dependencies -> Pipeline [FilePath]
concatModules config dependencies = liftF $ ConcatModules config dependencies id
