{-# LANGUAGE NamedFieldPuns #-}
module Interpreter.Pipeline
  (interpreter
  ) where

import CliArguments (readArguments)
import qualified Compile
import ConcatModule
import Config
import qualified Control.Concurrent.Async.Lifted as Concurrent
import Control.Monad.Free (foldFree)

import Control.Monad.State
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified DependencyTree
import qualified EntryPoints
import qualified Init
import qualified Logger
import Pipeline
import qualified ProgressBar
import System.FilePath ((<.>), (</>))
import Task (Env (..), Task, toTask)

interpreter :: PipelineF a -> Task a
interpreter command =
  case command of
    ReadCliArgs next                            -> next <$> toTask readArguments
    ReadConfig _ next                           -> next <$> Config.readConfig
    ReadDependencyCache config next             -> next <$> DependencyTree.readTreeCache (Config.temp_directory config)
    WriteDependencyCache config deps next       -> DependencyTree.writeTreeCache (Config.temp_directory config) deps >> return next
    FindEntryPoints config args next            -> next <$> EntryPoints.find config args
    FindDependency config cache entryPoint next -> next <$> DependencyTree.build config cache entryPoint
    Compile config toolPaths dep next           -> next <$> Compile.compile config toolPaths dep
    Init config next                            -> next <$> Init.setup config
    ConcatModule config dep next                -> next <$> ConcatModule.wrap config dep
    OutputCreatedModules config paths next      -> createdModulesJson config paths >> return next
    StartProgress title total next              -> ProgressBar.start total title >> return next
    EndProgress next                            -> ProgressBar.end >> return next
    AppendLog config msg next                   -> Logger.appendLog (log_directory config) msg >> return next
    ClearLog config next                        -> Logger.clearLog (log_directory config) >> return next
    Async commands next                         -> next <$> Concurrent.forConcurrently commands (foldFree interpreter)


-- writeLog :: FilePath -> Config ->  [T.Text] -> IO ()
-- writeLog fileName Config{log_directory} content = do
--   let logOutput = T.unlines content
--   _ <- writeFile (log_directory </> fileName) $ T.unpack logOutput
--   return ()

createdModulesJson :: Config.Config -> [FilePath] -> Task ()
createdModulesJson config paths = do
  let encodedPaths = Aeson.encode paths
  let jsonPath = Config.temp_directory config </> "modules" <.> "json"
  _ <- toTask $ BL.writeFile jsonPath encodedPaths
  _ <- ProgressBar.step
  return ()
