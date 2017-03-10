{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Logger
  ( interpreter
  -- , treeInterpreter
  ) where

import Data.Text as T
import Data.Tree as Tree
import Logger
import Pipeline

interpreter :: PipelineF a -> Log ()
interpreter (ReadCliArgs next)          = info "readCliArgs"
interpreter (ReadConfig maybePath next) = info "readConfig"
interpreter (Dependencies _ next)       = info "dependencies"
interpreter (Compile deps next)         = info "compile"

-- treeInterpreter :: PipelineF a -> Log ()
-- treeInterpreter (ReadCliArgs next) = info "readCliArgs"
-- treeInterpreter (ReadConfig maybePath next) = info "readConfig"
-- treeInterpreter (Dependencies _ next) = info "dependencies"
-- treeInterpreter (Compile deps next) =
--   info $ T.unlines $ fmap T.pack $ fmap (Tree.drawTree . fmap show) deps
