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

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified DependencyTree
import qualified EntryPoints
import qualified Init
import qualified Logger
import Pipeline
import System.Console.AsciiProgress
import System.FilePath ((<.>), (</>))
import Task (Task, toTask)

interpreter :: PipelineF a -> Task a
interpreter command =
  case command of
    ReadCliArgs next                          -> next <$> toTask readArguments
    ReadConfig _ next                         -> next <$> Config.readConfig
    ReadDependencyCache config next           -> next <$> DependencyTree.readTreeCache (Config.temp_directory config)
    WriteDependencyCache config deps next     -> DependencyTree.writeTreeCache (Config.temp_directory config) deps >> return next
    FindEntryPoints config args next          -> next <$> EntryPoints.find config args
    FindDependency pg config cache entryPoint next  -> next <$> DependencyTree.build pg config cache entryPoint
    Compile pg config toolPaths dep next      -> next <$> Compile.compile pg config toolPaths dep
    Init config next                          -> next <$> Init.setup config
    ConcatModule config dep next              -> next <$> ConcatModule.wrap config dep
    OutputCreatedModules pg config paths next -> createdModulesJson pg config paths >> return next
    StartProgress title total next            -> next <$> (toTask $ createProgress total title)
    EndProgress pg next                       -> (toTask $ complete pg) >> return next
    AppendLog config msg next                 -> Logger.appendLog (log_directory config) msg >> return next
    ClearLog config next                      -> Logger.clearLog (log_directory config) >> return next
    Async commands next                       -> next <$> Concurrent.forConcurrently commands (foldFree interpreter)


-- writeLog :: FilePath -> Config ->  [T.Text] -> IO ()
-- writeLog fileName Config{log_directory} content = do
--   let logOutput = T.unlines content
--   _ <- writeFile (log_directory </> fileName) $ T.unpack logOutput
--   return ()

createdModulesJson :: ProgressBar -> Config.Config -> [FilePath] -> Task ()
createdModulesJson pg config paths = toTask $ do
  let encodedPaths = Aeson.encode paths
  let jsonPath = Config.temp_directory config </> "modules" <.> "json"
  BL.writeFile jsonPath encodedPaths
  tick pg
  return ()

createProgress :: Int -> T.Text -> IO ProgressBar
createProgress total title = do
  newProgressBar def
      { pgTotal = toInteger total
      , pgOnCompletion = Just (T.unpack title ++ " finished after :elapsed seconds")
      , pgCompletedChar = '█'
      , pgPendingChar = '░'
      , pgFormat = T.unpack title ++ " ╢:bar╟ :current/:total"
      }
