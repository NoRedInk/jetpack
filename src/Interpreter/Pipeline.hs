{-# OPTIONS_GHC -Wall #-}

module Interpreter.Pipeline
  ( interpreter
  ) where

import qualified Config
import Pipeline

interpreter :: PipelineF a -> IO a
interpreter (ReadCliArgs next) =
  (putStrLn "readCliArgs") >> return (next noArgs)
interpreter (ReadConfig args next) =
  (putStrLn "readConfig") >> return (next Config.defaultConfig)
interpreter (Compile next) = (putStrLn "compile") >> return (next [])
