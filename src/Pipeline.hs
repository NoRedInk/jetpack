{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Pipeline where

import CliArguments (Args)
import Config (Config)
import Control.Monad.Free (Free, liftF)
import qualified Data.Text as T
import Dependencies (Dependencies, Dependency, DependencyTree)
import System.Console.AsciiProgress
import System.FilePath ()
import ToolPaths


data PipelineF next
  = ReadCliArgs (Args -> next)
  | ReadConfig (Maybe FilePath) (Config -> next)
  | FindEntryPoints Config Args ([FilePath] -> next)
  | Dependencies ProgressBar Config [FilePath] (Dependencies -> next)
  | Compile ProgressBar Config ToolPaths Dependency (T.Text -> next)
  | Init Config (ToolPaths -> next)
  | ConcatModule Config DependencyTree (FilePath -> next)
  | OutputCreatedModules ProgressBar Config [FilePath] next
  | StartProgress T.Text Int (ProgressBar -> next)
  | EndProgress ProgressBar next
  | AppendLog Config T.Text next
  | ClearLog Config next
  | forall a.Async [Pipeline a] ([a] -> next)

instance Functor PipelineF where
  fmap f (ReadCliArgs g) = ReadCliArgs (f . g)
  fmap f (ReadConfig maybeFilePath g) = ReadConfig maybeFilePath (f . g)
  fmap f (FindEntryPoints config args g) = FindEntryPoints config args (f . g)
  fmap f (Dependencies progressBar config paths g) = Dependencies progressBar config paths (f . g)
  fmap f (Compile progressBar config toolPaths dependency g) = Compile progressBar config toolPaths dependency (f . g)
  fmap f (Init config g) = Init config (f . g)
  fmap f (ConcatModule config dependencyTree g) = ConcatModule config dependencyTree (f . g)
  fmap f (OutputCreatedModules progressBar config paths next) = OutputCreatedModules progressBar config paths (f next)
  fmap f (StartProgress title total g) = StartProgress title total (f . g)
  fmap f (EndProgress progressBar next) = EndProgress progressBar (f next)
  fmap f (AppendLog config msg next) = AppendLog config msg (f next)
  fmap f (ClearLog config next) = ClearLog config (f next)
  fmap f (Async commands g) = Async commands (f . g)

type Pipeline = Free PipelineF

async :: [Pipeline a] -> Pipeline [a]
async commands = liftF $ Async commands id

readCliArgs :: Pipeline Args
readCliArgs = liftF $ ReadCliArgs id

readConfig :: Maybe FilePath -> Pipeline Config
readConfig maybePath = liftF $ ReadConfig maybePath id

findEntryPoints :: Config -> Args -> Pipeline [FilePath]
findEntryPoints config args = liftF $ FindEntryPoints config args id

dependencies :: ProgressBar -> Config -> [FilePath] -> Pipeline Dependencies
dependencies pg config entryPoints = liftF $ Dependencies pg config entryPoints id

compile :: ProgressBar -> Config -> ToolPaths -> Dependency -> Pipeline T.Text
compile pg config toolPaths dep = liftF $ Compile pg config toolPaths dep id

setup :: Config -> Pipeline ToolPaths
setup config = liftF $ Init config id

concatModule :: Config -> DependencyTree -> Pipeline FilePath
concatModule config dependency = liftF $ ConcatModule config dependency id

outputCreatedModules :: ProgressBar -> Config -> [FilePath] -> Pipeline ()
outputCreatedModules pg config paths = liftF $ OutputCreatedModules pg config paths ()

startProgress :: T.Text -> Int -> Pipeline ProgressBar
startProgress title total = liftF $ StartProgress title total id

endProgress :: ProgressBar -> Pipeline ()
endProgress pg = liftF $ EndProgress pg ()

appendLog :: Config -> T.Text -> Pipeline ()
appendLog config msg = liftF $ AppendLog config msg ()

clearLog :: Config -> Pipeline ()
clearLog config = liftF $ ClearLog config ()
