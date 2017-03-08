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
interpreter (Dependencies _ next) = info "dependencies"
interpreter (Compile _ next) = info "compile"
