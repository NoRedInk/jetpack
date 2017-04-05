{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline where

import CliArguments (Args)
import Config (Config)
import Control.Monad.Free (Free, liftF)
import Dependencies (Dependencies, Dependency)
import System.FilePath ()
import ToolPaths


data PipelineF next
  = ReadCliArgs (Args -> next)
  | ReadConfig (Maybe FilePath) (Config -> next)
  | FindEntryPoints Config Args ([FilePath] -> next)
  | Dependencies Config [FilePath] (Dependencies -> next)
  | Compile Config ToolPaths [Dependency] next
  | Init Config (ToolPaths -> next)
  | ConcatModules Config Dependencies ([FilePath] -> next)
  | OutputCreatedModules Config [FilePath] next
  deriving (Functor)

type Pipeline = Free PipelineF

readCliArgs :: Pipeline Args
readCliArgs = liftF $ ReadCliArgs id

readConfig :: Maybe FilePath -> Pipeline Config
readConfig maybePath = liftF $ ReadConfig maybePath id

findEntryPoints :: Config -> Args -> Pipeline [FilePath]
findEntryPoints config args = liftF $ FindEntryPoints config args id

dependencies :: Config -> [FilePath] -> Pipeline Dependencies
dependencies config entryPoints = liftF $ Dependencies config entryPoints id

compile :: Config -> ToolPaths -> [Dependency] -> Pipeline ()
compile config toolPaths deps = liftF $ Compile config toolPaths deps ()

setup :: Config -> Pipeline ToolPaths
setup config = liftF $ Init config id

concatModules :: Config -> Dependencies -> Pipeline [FilePath]
concatModules config dependencies = liftF $ ConcatModules config dependencies id

outputCreatedModules :: Config -> [FilePath] -> Pipeline ()
outputCreatedModules config paths = liftF $ OutputCreatedModules config paths ()
