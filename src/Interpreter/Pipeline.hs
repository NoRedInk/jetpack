{-# OPTIONS_GHC -Wall #-}

module Interpreter.Pipeline
  ( dryInterpreter
  , interpreter
  ) where

import qualified Config
import Control.Monad.Trans.Class (lift)
import qualified Dependencies
import Pipeline
import Task (Task)

interpreter :: PipelineF a -> Task a
interpreter (ReadCliArgs next) = lift (putStrLn "TODO") >> return (next noArgs)
interpreter (ReadConfig _ next) =
  lift (putStrLn "TODO") >> return (next Config.defaultConfig)
interpreter (Dependencies config next) = do
  deps <- Dependencies.find config
  return $ next deps
interpreter (Compile _ next) = lift (putStrLn "TODO") >> return (next [])

dryInterpreter :: PipelineF a -> Task a
dryInterpreter (ReadCliArgs next) =
  lift (putStrLn "reading cli arguments") >> return (next noArgs)
dryInterpreter (ReadConfig _ next) =
  lift (putStrLn "reading config") >> return (next Config.defaultConfig)
dryInterpreter (Dependencies _ next) =
  lift (putStrLn "finding all dependencies") >> return (next [])
dryInterpreter (Compile _ next) =
  lift (putStrLn "compiling") >> return (next [])
