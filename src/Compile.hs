{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
-}
module Compile where

import Config (Config (..))
import Control.Concurrent.Async.Lifted as Async
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.List as L
import Data.Text as T
import Data.Time.Clock
import Dependencies (Dependency (..))
import GHC.IO.Handle
import Parser.Ast as Ast
import System.Console.AsciiProgress
import System.Directory (copyFile)
import System.Exit
import System.FilePath ((</>))
import System.Process
import Task (Task)
import Utils.Files (pathToFileName)

newtype Compiler = Compiler { runCompiler :: FilePath -> FilePath -> Task [T.Text] }

compileModules :: Config -> [Dependency] -> Task ()
compileModules config@Config {log_directory} modules = do
  e <- lift $ displayConsoleRegions $ do
    pg <- newProgressBar def
      { pgTotal = toInteger $ L.length modules
      , pgOnCompletion = Just "Compiled :percent after :elapsed seconds"
      , pgCompletedChar = '█'
      , pgPendingChar = '░'
      , pgFormat = "Compiling ╢:bar╟ :current/:total"
      }
    runExceptT
      $ Async.forConcurrently modules
      $ compile pg config
  case e of
    Left err -> throwError err
    Right output  -> do
      let log = T.unlines $ fmap T.unlines output
      lift $ writeFile (log_directory </> "compile.log") $ T.unpack log
      return ()

{-| Compile a dependency.
 1. find compiler
 2. create output path
 3. compile to that output path
-}
compile :: ProgressBar -> Config -> Dependency -> Task [T.Text]
compile pg config (Dependency Ast.Elm _ p _)    = (runCompiler $ elmCompiler pg config) p $ buildArtifactPath config "js" p
compile pg config (Dependency Ast.Js _ p _)     = (runCompiler $ jsCompiler pg) p $ buildArtifactPath config "js" p
compile pg config (Dependency Ast.Coffee _ p _) = (runCompiler $ coffeeCompiler pg) p $ buildArtifactPath config "js" p -- todo get rid of ui here
compile pg config (Dependency Ast.Sass _ p _)   = (runCompiler $ sassCompiler pg) p $ buildArtifactPath config "css" p


buildArtifactPath :: Config -> String -> FilePath -> String
buildArtifactPath Config{temp_directory} extension inputPath =
  temp_directory </> pathToFileName inputPath extension

---------------
-- COMPILERS --
---------------

elmCompiler :: ProgressBar -> Config -> Compiler
elmCompiler pg Config{elm_root_directory} = Compiler $ \input output -> do
  -- TODO use elm_root_directory instead of the "../" below
  -- also pass in absolute paths
  let elmMake = "elm-make " ++ "../" ++ input ++ " --output " ++ "../" ++ output
  runCmd pg elmMake $ Just elm_root_directory

coffeeCompiler :: ProgressBar -> Compiler
coffeeCompiler pg = Compiler $ \input output -> do
  let coffee = "coffee -p " ++ input ++ " > " ++ output
  runCmd pg coffee Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: ProgressBar -> Compiler
jsCompiler pg = Compiler $ \input output -> lift $ do
  copyFile input output
  tick pg
  currentTime <- getCurrentTime
  let commandFinished = T.pack $ show currentTime
  return [commandFinished, T.concat ["moved ", T.pack input, " => ", T.pack output]]

sassCompiler :: ProgressBar -> Compiler
sassCompiler pg = Compiler $ \input output -> do
  let sassc = "sassc " ++ input ++ " " ++ output ++ " --load-path " ++ loadPath -- <= include stuff that is needed in load-path
  runCmd pg sassc Nothing
  where
    loadPath = L.intercalate ":"
      -- TODO move this to config
      [ "node_modules"
      , "vendor" </> "assets" </> "components" </> "animatewithsass"
      , "app" </> "assets" </> "modules" </> "css"
      , "app" </> "assets" </> "stylesheets" </> "webpack"
      , "ui" </> "src"
      , "node_modules" </> "bourbon" </> "app" </> "assets" </> "stylesheets"
      , "node_modules" </> "bourbon-neat" </> "app" </> "assets" </> "stylesheets"
      ]

runCmd :: ProgressBar -> String -> Maybe String -> Task [T.Text]
runCmd pg cmd maybeCwd = do
  -- TODO: handle exit status here
  (_, Just out, _, ph) <- lift $ createProcess (proc "bash" ["-c", cmd])
    { std_out = CreatePipe
    , cwd = maybeCwd
    }
  ec <- lift $ waitForProcess ph
  case ec of
      ExitSuccess   -> lift $ do
        content <- hGetContents out
        tick pg
        currentTime <- getCurrentTime
        let commandFinished = T.pack $ show currentTime
        return [commandFinished, T.pack cmd, T.pack content]
      ExitFailure _ -> throwError []
