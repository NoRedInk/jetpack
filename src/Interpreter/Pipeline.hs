{-# OPTIONS_GHC -Wall #-}

module Interpreter.Pipeline
  ( interpreter
  ) where

import qualified Config
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Error
import Pipeline
import Task (Task)

interpreter :: PipelineF a -> Task a
interpreter (ReadCliArgs next) =
  lift (putStrLn "readCliArgs") >> return (next noArgs)
interpreter (ReadConfig args next) =
  lift (putStrLn "readConfig") >> return (next Config.defaultConfig)
interpreter (Compile next) = right (putStrLn "compile") >> return (next [])
