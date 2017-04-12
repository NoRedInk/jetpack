{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

{-| This is our grammar to build jetpack.
-}
module Algebra.Pipeline where

import CliArguments (Args)
import Control.Monad.Free (Free, liftF)
import qualified Data.Text as T
import Dependencies (Dependencies, Dependency, DependencyTree)
import System.FilePath ()
import ToolPaths

data PipelineF next
  = ReadDependencyCache (Dependencies -> next)
  | WriteDependencyCache Dependencies next
  | FindEntryPoints Args ([FilePath] -> next)
  | FindDependency Dependencies FilePath (DependencyTree -> next)
  | Compile ToolPaths Dependency (T.Text -> next)
  | ConcatModule DependencyTree (FilePath -> next)
  | OutputCreatedModules [FilePath] next
  | StartProgress T.Text Int next
  | EndProgress next
  | AppendLog T.Text next
  | forall a.Async [Pipeline a] ([a] -> next)

instance Functor PipelineF where
  fmap f (ReadDependencyCache g) = ReadDependencyCache (f . g)
  fmap f (WriteDependencyCache deps next) = WriteDependencyCache deps (f next)
  fmap f (FindEntryPoints args g) = FindEntryPoints args (f . g)
  fmap f (FindDependency cache path g) = FindDependency cache path (f . g)
  fmap f (Compile toolPaths dependency g) = Compile toolPaths dependency (f . g)
  fmap f (ConcatModule dependencyTree g) = ConcatModule dependencyTree (f . g)
  fmap f (OutputCreatedModules paths next) = OutputCreatedModules paths (f next)
  fmap f (StartProgress title total next) = StartProgress title total (f next)
  fmap f (EndProgress next) = EndProgress (f next)
  fmap f (AppendLog msg next) = AppendLog msg (f next)
  fmap f (Async commands g) = Async commands (f . g)

type Pipeline = Free PipelineF

-- Helper functions to create a `Free PiplineF`
--

async :: [Pipeline a] -> Pipeline [a]
async commands = liftF $ Async commands id

readDependencyCache :: Pipeline Dependencies
readDependencyCache = liftF $ ReadDependencyCache id

writeDependencyCache :: Dependencies -> Pipeline ()
writeDependencyCache deps = liftF $ WriteDependencyCache deps ()

findEntryPoints :: Args -> Pipeline [FilePath]
findEntryPoints args = liftF $ FindEntryPoints args id

findDependency :: Dependencies -> FilePath -> Pipeline DependencyTree
findDependency cache entryPoint = liftF $ FindDependency cache entryPoint id

compile :: ToolPaths -> Dependency -> Pipeline T.Text
compile toolPaths dep = liftF $ Compile toolPaths dep id

concatModule :: DependencyTree -> Pipeline FilePath
concatModule dependency = liftF $ ConcatModule dependency id

outputCreatedModules :: [FilePath] -> Pipeline ()
outputCreatedModules paths = liftF $ OutputCreatedModules paths ()

startProgress :: T.Text -> Int -> Pipeline ()
startProgress title total = liftF $ StartProgress title total ()

endProgress :: Pipeline ()
endProgress = liftF $ EndProgress ()

appendLog :: T.Text -> Pipeline ()
appendLog  msg = liftF $ AppendLog msg ()
