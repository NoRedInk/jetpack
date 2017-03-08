{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Dependencies
  ( find
  , Dependencies
  ) where

import Config
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Parser.Ast as Ast
import qualified Parser.Require
import System.FilePath ()
import Task (Task)
import Utils.Files (findAllFilesIn)

data Dependency = Dependency
  { path :: FilePath
  , requires :: [Ast.Require]
  } deriving (Show)

type Dependencies = [Dependency]

{-| Find all dependencies for files in `module_directory`.
-}
find :: Config -> Task Dependencies
find config = do
  paths <- findAllFilesIn $ Config.module_directory config
  traverse findRequires paths

findRequires :: FilePath -> Task Dependency
findRequires path = do
  content <- lift $ readFile path
  return $ Dependency path $ Parser.Require.jsRequires $ T.pack content
