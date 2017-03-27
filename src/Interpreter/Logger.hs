{-# LANGUAGE OverloadedStrings #-}

module Interpreter.Logger
  ( interpreter
  -- , treeInterpreter
  ) where

-- import Data.Text as T
-- import Data.Tree as Tree
import qualified Logger
import Pipeline

-- TODO log somethign useful
interpreter :: PipelineF a -> Logger.Log ()
interpreter (ReadCliArgs _)       = Logger.info "readCliArgs"
interpreter (ReadConfig _ _)      = Logger.info "readConfig"
interpreter (Dependencies _ _)    = Logger.info "dependencies"
interpreter (Compile _ _ _ _)     = Logger.info "compile"
interpreter (Init _ _)            = Logger.info "setup"
interpreter (ConcatModules _ _ _) = Logger.info "concatModules"

-- treeInterpreter :: PipelineF a -> Log ()
-- treeInterpreter (ReadCliArgs next) = info "readCliArgs"
-- treeInterpreter (ReadConfig maybePath next) = info "readConfig"
-- treeInterpreter (Dependencies _ next) = info "dependencies"
-- treeInterpreter (Compile deps next) =
--   info $ T.unlines $ fmap T.pack $ fmap (Tree.drawTree . fmap show) deps
