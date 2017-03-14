{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
-}
module Compile where

import Control.Concurrent.Async.Lifted as Async
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Data.List as L
import Data.Text as T
import Dependencies (Dependency (..))
import Error (Error (..))
import GHC.IO.Handle
import Parser.Ast as Ast
import System.Exit
import System.FilePath ((<.>), (</>))
import System.Process
import Task (Task)
import Utils.Files (pathToFileName)

newtype Compiler = Compiler { runCompiler :: FilePath -> FilePath -> Task () }

compileModules :: [Dependency] -> Task ()
compileModules modules = Async.forConcurrently_ modules compile

{-| Compile a dependency.
 1. find compiler
 2. create output path
 3. compile to that output path
-}
compile :: Dependency -> Task ()
compile (Dependency Ast.Elm _ p)    = (runCompiler elmCompiler) p $ outputFileName p
compile (Dependency Ast.Js _ p)     = (runCompiler jsCompiler) p "test"
compile (Dependency Ast.Coffee _ p) = (runCompiler coffeeCompiler) p $ "ui" </> outputFileName p
compile (Dependency Ast.Sass _ p)   = (runCompiler sassCompiler) p $ sassOutputFileName p


outputFileName :: FilePath -> String
outputFileName path =
  "."
  </> "tmp" -- TODO use path from config
  </> (T.unpack $ pathToFileName path)
  <.> "js"

sassOutputFileName :: FilePath -> String
sassOutputFileName path =
  "."
  </> "ui" -- TODO use path from config
  </> "tmp" -- TODO use path from config
  </> (T.unpack $ pathToFileName path)
  <.> "css"

---------------
-- COMPILERS --
---------------

elmCompiler :: Compiler
elmCompiler = Compiler $ \input output -> do
  let elmMake = "elm-make " ++ "../" ++ input ++ " --output " ++ output
  runCmd elmMake "./ui" -- TODO use path from config

coffeeCompiler :: Compiler
coffeeCompiler = Compiler $ \input output -> do
  runCoffee input output

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: Compiler
jsCompiler = Compiler $ \_input _output -> do
  (_, maybeOut, _, _) <- lift $ createProcess (proc "echo" ["JS"]){ std_out = CreatePipe }
  printStdOut maybeOut
  return ()

sassCompiler :: Compiler
sassCompiler = Compiler $ \input output -> do
  let sassc = "sassc " ++ input ++ " " ++ output ++ " --load-path " ++ loadPath -- <= include stuff that is needed in load-path
  runCmd sassc "./" -- TODO use path from config
  where
    loadPath = L.intercalate (":")
      -- TODO move this to config
      [ "node_modules"
      , "vendor" </> "assets" </> "components" </> "animatewithsass"
      , "app" </> "assets" </> "modules" </> "css"
      , "app" </> "assets" </> "stylesheets" </> "webpack"
      , "ui" </> "src"
      , "node_modules" </> "bourbon" </> "app" </> "assets" </> "stylesheets"
      , "node_modules" </> "bourbon-neat" </> "app" </> "assets" </> "stylesheets"
      ]

runCmd :: String -> String -> Task ()
runCmd cmd cwd = do
  (_, maybeOut, _, _) <- lift $ createProcess (proc "bash" ["-c", cmd])
    { std_out = CreatePipe
    , cwd = Just cwd
    }
  printStdOut maybeOut
  return ()

runCoffee :: String -> String -> Task ()
runCoffee input output = do
  let cmd = "coffee -p " ++ input ++ " > " ++ output
  (e,_,_) <- lift $ readProcessWithExitCode "bash" ["-c", cmd] ""
  if e == ExitSuccess then
    return ()
  else
    throwError [JsonInvalid input]

printStdOut :: Maybe Handle -> Task ()
printStdOut (Just out) = lift $ do
                  contents <- hGetContents out
                  -- putStrLn contents
                  return ()
printStdOut Nothing = lift $ do return ()
