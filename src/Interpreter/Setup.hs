{-# LANGUAGE NamedFieldPuns #-}
module Interpreter.Setup
  (interpreter
  ) where

import CliArguments (readArguments)
import Config

import Algebra.Setup
import qualified Init
import qualified Logger
import Task (Task, toTask)

interpreter :: SetupF a -> Task a
interpreter command =
  case command of
    ReadCliArgs next  -> next <$> toTask readArguments
    ReadConfig _ next -> next <$> Config.readConfig
    Setup next        -> next <$> Init.setup
    ClearLog next     -> Logger.clearLog  >> return next
