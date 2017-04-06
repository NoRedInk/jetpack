{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline where

import CliArguments (Args)
import Config (Config)
import Control.Monad.Free (Free, liftF)
import qualified Data.Text as T
import Dependencies (Dependencies, Dependency)
import System.Console.AsciiProgress
import System.FilePath ()
import ToolPaths


data PipelineF next
  = ReadCliArgs (Args -> next)
  | ReadConfig (Maybe FilePath) (Config -> next)
  | FindEntryPoints Config Args ([FilePath] -> next)
  | Dependencies ProgressBar Config [FilePath] (Dependencies -> next)
  | Compile ProgressBar Config ToolPaths Dependency next
  | Init Config (ToolPaths -> next)
  | ConcatModules Config Dependencies ([FilePath] -> next)
  | OutputCreatedModules Config [FilePath] next
  | StartProgress T.Text Int (ProgressBar -> next)
  | EndProgress ProgressBar next
  deriving (Functor)

type Pipeline = Free PipelineF

readCliArgs :: Pipeline Args
readCliArgs = liftF $ ReadCliArgs id

readConfig :: Maybe FilePath -> Pipeline Config
readConfig maybePath = liftF $ ReadConfig maybePath id

findEntryPoints :: Config -> Args -> Pipeline [FilePath]
findEntryPoints config args = liftF $ FindEntryPoints config args id

dependencies :: ProgressBar -> Config -> [FilePath] -> Pipeline Dependencies
dependencies pg config entryPoints = liftF $ Dependencies pg config entryPoints id

compile :: ProgressBar -> Config -> ToolPaths -> Dependency -> Pipeline ()
compile pg config toolPaths dep = liftF $ Compile pg config toolPaths dep ()

setup :: Config -> Pipeline ToolPaths
setup config = liftF $ Init config id

concatModules :: Config -> Dependencies -> Pipeline [FilePath]
concatModules config dependencies = liftF $ ConcatModules config dependencies id

outputCreatedModules :: Config -> [FilePath] -> Pipeline ()
outputCreatedModules config paths = liftF $ OutputCreatedModules config paths ()

startProgress :: T.Text -> Int -> Pipeline ProgressBar
startProgress title total = liftF $ StartProgress title total id

endProgress :: ProgressBar -> Pipeline ()
endProgress pg = liftF $ EndProgress pg ()
