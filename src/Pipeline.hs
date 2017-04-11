{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Pipeline where

import CliArguments (Args)
import Config (Config)
import Control.Monad.Free (Free, liftF)
import qualified Data.Text as T
import Dependencies (Dependencies, Dependency, DependencyTree)
import System.FilePath ()
import ToolPaths


data PipelineF next
  = ReadCliArgs (Args -> next)
  | ReadConfig (Maybe FilePath) (Config -> next)
  | ReadDependencyCache Config (Dependencies -> next)
  | WriteDependencyCache Config Dependencies next
  | FindEntryPoints Config Args ([FilePath] -> next)
  | FindDependency Config Dependencies FilePath (DependencyTree -> next)
  | Compile Config ToolPaths Dependency (T.Text -> next)
  | Init Config (ToolPaths -> next)
  | ConcatModule Config DependencyTree (FilePath -> next)
  | OutputCreatedModules Config [FilePath] next
  | StartProgress T.Text Int next
  | EndProgress next
  | AppendLog Config T.Text next
  | ClearLog Config next
  | forall a.Async [Pipeline a] ([a] -> next)

instance Functor PipelineF where
  fmap f (ReadCliArgs g) = ReadCliArgs (f . g)
  fmap f (ReadConfig maybeFilePath g) = ReadConfig maybeFilePath (f . g)
  fmap f (ReadDependencyCache config g) = ReadDependencyCache config (f . g)
  fmap f (WriteDependencyCache config deps next) = WriteDependencyCache config deps (f next)
  fmap f (FindEntryPoints config args g) = FindEntryPoints config args (f . g)
  fmap f (FindDependency config cache path g) = FindDependency config cache path (f . g)
  fmap f (Compile config toolPaths dependency g) = Compile config toolPaths dependency (f . g)
  fmap f (Init config g) = Init config (f . g)
  fmap f (ConcatModule config dependencyTree g) = ConcatModule config dependencyTree (f . g)
  fmap f (OutputCreatedModules config paths next) = OutputCreatedModules config paths (f next)
  fmap f (StartProgress title total next) = StartProgress title total (f next)
  fmap f (EndProgress next) = EndProgress (f next)
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

readDependencyCache :: Config -> Pipeline Dependencies
readDependencyCache config = liftF $ ReadDependencyCache config id

writeDependencyCache :: Config -> Dependencies -> Pipeline ()
writeDependencyCache config deps = liftF $ WriteDependencyCache config deps ()

findEntryPoints :: Config -> Args -> Pipeline [FilePath]
findEntryPoints config args = liftF $ FindEntryPoints config args id

findDependency :: Config -> Dependencies -> FilePath -> Pipeline DependencyTree
findDependency config cache entryPoint = liftF $ FindDependency config cache entryPoint id

compile :: Config -> ToolPaths -> Dependency -> Pipeline T.Text
compile config toolPaths dep = liftF $ Compile config toolPaths dep id

setup :: Config -> Pipeline ToolPaths
setup config = liftF $ Init config id

concatModule :: Config -> DependencyTree -> Pipeline FilePath
concatModule config dependency = liftF $ ConcatModule config dependency id

outputCreatedModules :: Config -> [FilePath] -> Pipeline ()
outputCreatedModules config paths = liftF $ OutputCreatedModules config paths ()

startProgress :: T.Text -> Int -> Pipeline ()
startProgress title total = liftF $ StartProgress title total ()

endProgress :: Pipeline ()
endProgress = liftF $ EndProgress ()

appendLog :: Config -> T.Text -> Pipeline ()
appendLog config msg = liftF $ AppendLog config msg ()

clearLog :: Config -> Pipeline ()
clearLog config = liftF $ ClearLog config ()
