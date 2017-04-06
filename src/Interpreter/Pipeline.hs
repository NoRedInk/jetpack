{-# LANGUAGE NamedFieldPuns #-}
module Interpreter.Pipeline
  (interpreter
  ) where

import CliArguments (readArguments)
import qualified Compile
import ConcatModule
import Config
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Text as T
import qualified DependencyTree
import qualified EntryPoints
import qualified Init
import Pipeline
import System.Console.AsciiProgress
import System.FilePath ((<.>), (</>))
import Task (Task)

interpreter :: PipelineF a -> Task a
interpreter command =
  case command of
    ReadCliArgs next                       -> next <$> lift readArguments
    ReadConfig _ next                      -> next <$> Config.readConfig
    FindEntryPoints config args next       -> next <$> EntryPoints.find config args
    Dependencies config entryPoints next          -> next <$> DependencyTree.build config entryPoints
    Compile config toolPaths deps next     -> do
      let total = L.length deps
      output <- withProgress total command $ Compile.compileModules config toolPaths deps
      _ <- lift $ writeLog "compile.log" config output
      return next
    Init config next                       -> next <$> Init.setup config
    ConcatModules config dependencies next -> next <$> ConcatModule.wrap config dependencies
    OutputCreatedModules config paths next -> createdModulesJson config paths >> return next


writeLog :: FilePath -> Config ->  [T.Text] -> IO ()
writeLog fileName Config{log_directory} content = do
  let logOutput = T.unlines content
  _ <- writeFile (log_directory </> fileName) $ T.unpack logOutput
  return ()

createdModulesJson :: Config.Config -> [FilePath] -> Task ()
createdModulesJson config paths = lift $ do
  let encodedPaths = Aeson.encode paths
  let jsonPath = Config.temp_directory config </> "modules" <.> "json"
  BL.writeFile jsonPath encodedPaths
  return ()

withProgress :: Int -> PipelineF a -> (ProgressBar -> Task [T.Text]) -> Task [T.Text]
withProgress total command task =
  ExceptT
  $ displayConsoleRegions
  $ do
    pg <- createProgress total command
    e <- runExceptT (task pg)
    complete pg
    return e

createProgress :: Int -> PipelineF a -> IO ProgressBar
createProgress total command = do
  let title = progressTitle command
  newProgressBar def
      { pgTotal = toInteger total
      , pgOnCompletion = Just (title ++ " :percent after :elapsed seconds")
      , pgCompletedChar = '█'
      , pgPendingChar = '░'
      , pgFormat = title ++ " ╢:bar╟ :current/:total"
      }

progressTitle :: PipelineF a -> String
progressTitle command =
  case command of
    ReadCliArgs _              -> "ReadCliArgs"
    ReadConfig _ _             -> "ReadConfig"
    FindEntryPoints _ _ _      -> "FindEntryPoints"
    Dependencies _ _ _         -> "Dependencies"
    Compile _ _ _ _            -> "Compile"
    Init _ _                   -> "Init"
    ConcatModules _ _ _        -> "ConcatModules"
    OutputCreatedModules _ _ _ -> "OutputCreatedModules"
