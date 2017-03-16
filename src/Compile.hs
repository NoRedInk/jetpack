{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
-}
module Compile where

import Config (Config (..))
import Control.Concurrent.Async.Lifted as Async
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Data.List as L
import Data.Text as T
import Dependencies (Dependency (..))
import GHC.IO.Handle
import Parser.Ast as Ast
import System.Directory (copyFile)
import System.Exit
import System.FilePath ((<.>), (</>))
import System.Process
import Task (Task)
import Utils.Files (pathToFileName)

newtype Compiler = Compiler { runCompiler :: FilePath -> FilePath -> Task () }

compileModules :: Config -> [Dependency] -> Task ()
compileModules config modules = do
  -- TODO be careful with this
  Async.forConcurrently_ modules $ compile config
  -- traverse (compile config) modules
  -- return ()

{-| Compile a dependency.
 1. find compiler
 2. create output path
 3. compile to that output path
-}
compile :: Config -> Dependency -> Task ()
compile config (Dependency Ast.Elm _ p _)    = (runCompiler $ elmCompiler config) p $ buildArtifactPath config "js" p
compile config (Dependency Ast.Js _ p _)     = (runCompiler jsCompiler) p $ buildArtifactPath config "js" p
compile config (Dependency Ast.Coffee _ p _) = (runCompiler coffeeCompiler) p $ buildArtifactPath config "js" p -- todo get rid of ui here
compile config (Dependency Ast.Sass _ p _)   = (runCompiler sassCompiler) p $ buildArtifactPath config "css" p


buildArtifactPath :: Config -> String -> FilePath -> String
buildArtifactPath Config{temp_directory} extension inputPath =
  temp_directory </> pathToFileName inputPath extension

---------------
-- COMPILERS --
---------------

elmCompiler :: Config -> Compiler
elmCompiler Config{elm_root_directory} = Compiler $ \input output -> do
  -- TODO use elm_root_directory instead of the "../" below
  -- also pass in absolute paths
  let elmMake = "elm-make " ++ "../" ++ input ++ " --output " ++ "../" ++ output
  runCmd elmMake $ Just elm_root_directory

coffeeCompiler :: Compiler
coffeeCompiler = Compiler $ \input output -> do
  let coffee = "coffee -p " ++ input ++ " > " ++ output
  runCmd coffee Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: Compiler
jsCompiler = Compiler $ \input output -> lift $ copyFile input output


sassCompiler :: Compiler
sassCompiler = Compiler $ \input output -> do
  let sassc = "sassc " ++ input ++ " " ++ output ++ " --load-path " ++ loadPath -- <= include stuff that is needed in load-path
  runCmd sassc Nothing
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

runCmd :: String -> Maybe String -> Task ()
runCmd cmd maybeCwd = do
  -- TODO: handle exit status here
  (_, Just out, _, ph) <- lift $ createProcess (proc "bash" ["-c", cmd])
    { std_out = CreatePipe
    , cwd = maybeCwd
    }
  ec <- lift $ waitForProcess ph
  case ec of
      ExitSuccess   -> lift $ do
        content <- hGetContents out
        if content /= "" then do
          _ <- putStrLn content
          return ()
        else
          return ()
      ExitFailure _ -> throwError []
