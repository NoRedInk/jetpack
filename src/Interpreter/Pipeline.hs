{-# OPTIONS_GHC -Wall #-}

module Interpreter.Pipeline
  ( dryInterpreter
  , interpreter
  ) where

import qualified Compile
import ConcatModule
import qualified Config
import Control.Monad.Trans.Class (lift)
import qualified Dependencies
import qualified Init
import Pipeline
import Task (Task)

interpreter :: PipelineF a -> Task a
interpreter (ReadCliArgs next) = lift (putStrLn "TODO") >> return (next noArgs)
interpreter (ReadConfig _ next) = lift (putStrLn "TODO") >> return (next Config.defaultConfig)
interpreter (Dependencies config next) = do
  deps <- Dependencies.find config
  return $ next deps
interpreter (Compile config deps next) = do
  Compile.compileModules config deps
  return next
interpreter (Init config next) = do
  _ <- Init.setup config
  return next
interpreter (ConcatModules config dependencies next) = do
  outputPaths <- ConcatModule.wrap config dependencies
  return (next outputPaths)

dryInterpreter :: PipelineF a -> Task a
dryInterpreter (ReadCliArgs next) = lift (putStrLn "reading cli arguments") >> return (next noArgs)
dryInterpreter (ReadConfig _ next) = lift (putStrLn "reading config") >> return (next Config.defaultConfig)
dryInterpreter (Dependencies _ next) = lift (putStrLn "finding all dependencies") >> return (next [])
dryInterpreter (Compile _ _ next) = lift (putStrLn "compiling") >> return next
dryInterpreter (Init _ next) = lift (putStrLn "setting up") >> return next
dryInterpreter (ConcatModules _ _ next) = lift (putStrLn "concating modules") >> return (next [])
