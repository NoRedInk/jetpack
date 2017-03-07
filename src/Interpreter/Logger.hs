{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Logger
  ( interpreter
  ) where

import Logger
import Pipeline

interpreter :: PipelineF a -> Log ()
interpreter (ReadCliArgs next) = info "readCliArgs"
interpreter (ReadConfig maybePath next) = info "readConfig"
interpreter (Compile next) = info "compile"
