{-# LANGUAGE NamedFieldPuns #-}
module Interpreter.Pipeline
  (interpreter
  ) where

import CliArguments (readArguments)
import qualified Compile
import ConcatModule
import Config
import Control.Monad.Trans.Class (lift)
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
import Task (Task)

interpreter :: PipelineF a -> Task a
interpreter command =
  case command of
    ReadCliArgs next                          -> next <$> lift readArguments
    ReadConfig _ next                         -> next <$> Config.readConfig
    FindEntryPoints config args next          -> next <$> EntryPoints.find config args
    Dependencies pg config entryPoints next   -> next <$> DependencyTree.build pg config entryPoints
    Compile pg config toolPaths dep next      -> next <$> Compile.compile pg config toolPaths dep
    Init config next                          -> next <$> Init.setup config
    ConcatModule config dep next              -> next <$> ConcatModule.wrap config dep
    OutputCreatedModules pg config paths next -> createdModulesJson pg config paths >> return next
    StartProgress title total next            -> next <$> (lift $ createProgress total title)
    EndProgress pg next                       -> (lift $ complete pg) >> return next
    AppendLog config msg next                 -> Logger.appendLog (log_directory config) msg >> return next
    ClearLog config next                      -> Logger.clearLog (log_directory config) >> return next


-- writeLog :: FilePath -> Config ->  [T.Text] -> IO ()
-- writeLog fileName Config{log_directory} content = do
--   let logOutput = T.unlines content
--   _ <- writeFile (log_directory </> fileName) $ T.unpack logOutput
--   return ()

createdModulesJson :: ProgressBar -> Config.Config -> [FilePath] -> Task ()
createdModulesJson pg config paths = lift $ do
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
