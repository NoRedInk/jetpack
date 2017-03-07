{-# OPTIONS_GHC -Wall #-}

module Interpreter.Pipeline
  ( interpreter
  ) where

import qualified Config
import Pipeline

interpreter :: PipelineF a -> Runnable a
interpreter (ReadCliArgs next) =
  printStr (putStrLn "readCliArgs") >> return (next noArgs)
interpreter (ReadConfig args next) =
  printStr (putStrLn "readConfig") >> return (next Config.defaultConfig)
interpreter (Compile next) = printStr (putStrLn "compile") >> return (next [])
