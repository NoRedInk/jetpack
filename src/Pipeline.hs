{-# LANGUAGE ExistentialQuantification #-}

{-| This is our grammar to build jetpack.
-}
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
  | ReadConfig (Maybe FilePath)
               (Config -> next)
  | ReadDependencyCache (Dependencies -> next)
  | WriteDependencyCache Dependencies
                         next
  | FindEntryPoints ([FilePath] -> next)
  | FindDependency Dependencies
                   FilePath
                   (DependencyTree -> next)
  | Compile ToolPaths
            Dependency
            ((T.Text, Maybe T.Text) -> next)
  | Init (ToolPaths -> next)
  | ConcatModule DependencyTree
                 (FilePath -> next)
  | OutputCreatedModules [FilePath]
                         next
  | StartProgress T.Text
                  Int
                  next
  | EndProgress next
  | StartSpinner T.Text
                 next
  | EndSpinner T.Text
               next
  | AppendLog T.Text
              T.Text
              next
  | ClearLog T.Text
             next
  | Hook String
         (T.Text -> next)
  | Version (T.Text -> next)
  | forall a. Async [Pipeline a]
                    ([a] -> next)

instance Functor PipelineF where
  fmap f (ReadCliArgs g) = ReadCliArgs (f . g)
  fmap f (ReadConfig maybeFilePath g) = ReadConfig maybeFilePath (f . g)
  fmap f (ReadDependencyCache g) = ReadDependencyCache (f . g)
  fmap f (WriteDependencyCache deps next) = WriteDependencyCache deps (f next)
  fmap f (FindEntryPoints g) = FindEntryPoints (f . g)
  fmap f (FindDependency cache path g) = FindDependency cache path (f . g)
  fmap f (Compile toolPaths dependency g) = Compile toolPaths dependency (f . g)
  fmap f (Init g) = Init (f . g)
  fmap f (ConcatModule dependencyTree g) = ConcatModule dependencyTree (f . g)
  fmap f (OutputCreatedModules paths next) = OutputCreatedModules paths (f next)
  fmap f (StartProgress title total next) = StartProgress title total (f next)
  fmap f (EndProgress next) = EndProgress (f next)
  fmap f (StartSpinner title next) = StartSpinner title (f next)
  fmap f (EndSpinner title next) = EndSpinner title (f next)
  fmap f (AppendLog fileName msg next) = AppendLog fileName msg (f next)
  fmap f (ClearLog fileName next) = ClearLog fileName (f next)
  fmap f (Hook hookScript g) = Hook hookScript (f . g)
  fmap f (Version g) = Version (f . g)
  fmap f (Async commands g) = Async commands (f . g)

type Pipeline = Free PipelineF

-- Helper functions to create a `Free PiplineF`
--
async :: [Pipeline a] -> Pipeline [a]
async commands = liftF $ Async commands id

readCliArgs :: Pipeline Args
readCliArgs = liftF $ ReadCliArgs id

readConfig :: Maybe FilePath -> Pipeline Config
readConfig maybePath = liftF $ ReadConfig maybePath id

readDependencyCache :: Pipeline Dependencies
readDependencyCache = liftF $ ReadDependencyCache id

writeDependencyCache :: Dependencies -> Pipeline ()
writeDependencyCache deps = liftF $ WriteDependencyCache deps ()

findEntryPoints :: Pipeline [FilePath]
findEntryPoints = liftF $ FindEntryPoints id

findDependency :: Dependencies -> FilePath -> Pipeline DependencyTree
findDependency cache entryPoint = liftF $ FindDependency cache entryPoint id

compile :: ToolPaths -> Dependency -> Pipeline (T.Text, Maybe T.Text)
compile toolPaths dep = liftF $ Compile toolPaths dep id

setup :: Pipeline ToolPaths
setup = liftF $ Init id

concatModule :: DependencyTree -> Pipeline FilePath
concatModule dependency = liftF $ ConcatModule dependency id

outputCreatedModules :: [FilePath] -> Pipeline ()
outputCreatedModules paths = liftF $ OutputCreatedModules paths ()

startProgress :: T.Text -> Int -> Pipeline ()
startProgress title total = liftF $ StartProgress title total ()

endProgress :: Pipeline ()
endProgress = liftF $ EndProgress ()

startSpinner :: T.Text -> Pipeline ()
startSpinner title = liftF $ StartSpinner title ()

endSpinner :: T.Text -> Pipeline ()
endSpinner title = liftF $ EndSpinner title ()

appendLog :: T.Text -> T.Text -> Pipeline ()
appendLog fileName msg = liftF $ AppendLog fileName msg ()

clearLog :: T.Text -> Pipeline ()
clearLog fileName = liftF $ ClearLog fileName ()

hook :: String -> Pipeline T.Text
hook hookScript = liftF $ Hook hookScript id

version :: Pipeline T.Text
version = liftF $ Version id
