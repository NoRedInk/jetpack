{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
-}
module Compile where

import qualified Config
import Control.Monad.Trans.Class (lift)
import Dependencies (Dependency (..))
import qualified Error
import GHC.IO.Handle
import Parser.Ast as Ast
import System.FilePath ()
import System.Process
import Task (Task)


newtype Compiler = Compiler { runCompiler :: FilePath -> FilePath -> Task () }

compileModules :: [Dependency] -> Task ()
compileModules modules = do
  traverse compile modules
  return ()

{-| Compile a dependency.
 1. find compiler
 2. create output path
 3. compile to that output path
-}
compile :: Dependency -> Task ()
compile (Dependency Ast.Elm _ p)    = (runCompiler elmCompiler) p "test"
compile (Dependency Ast.Js _ p)     = (runCompiler jsCompiler) p "test"
compile (Dependency Ast.Coffee _ p) = (runCompiler coffeeCompiler) p "test"
compile (Dependency Ast.Sass _ p)   = (runCompiler sassCompiler) p "test"


---------------
-- COMPILERS --
---------------

elmCompiler :: Compiler
elmCompiler = Compiler $ \input output -> do
  (_, maybeOut, _, _) <- lift $ createProcess (proc "echo" ["ELM"]){ std_out = CreatePipe }
  printStdOut maybeOut
  return ()

coffeeCompiler :: Compiler
coffeeCompiler = Compiler $ \input output -> do
  (_, maybeOut, _, _) <- lift $ createProcess (proc "echo" ["COFFEE"]){ std_out = CreatePipe }
  printStdOut maybeOut
  return ()

jsCompiler :: Compiler
jsCompiler = Compiler $ \input output -> do
  (_, maybeOut, _, _) <- lift $ createProcess (proc "echo" ["JS"]){ std_out = CreatePipe }
  printStdOut maybeOut
  return ()

sassCompiler :: Compiler
sassCompiler = Compiler $ \input output -> do
  (_, maybeOut, _, _) <- lift $ createProcess (proc "echo" ["SASS"]){ std_out = CreatePipe }
  printStdOut maybeOut
  return ()

printStdOut :: Maybe Handle -> Task ()
printStdOut (Just out) = lift $ do
                  contents <- hGetContents out
                  putStrLn contents
printStdOut Nothing = lift $ do return ()
