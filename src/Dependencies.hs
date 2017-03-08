{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Finds all dependencies of a module. It creates a try like the following for each module.
  ```
  (Require "./app/assets/modules/js/Super/foo_bar.js" Js)
  |
  +- (Require "Page/Super/FooBar/Main.elm" Elm)
  |
  `- (Require "Page/Super/FooBar/index.coffee" Coffee)
    |
    `- (Require "lodash" Js)
  ```
-}
module Dependencies
  ( find
  , Dependencies
  ) where

import Config
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import qualified Data.Tree as Tree
import Parser.Ast as Ast
import qualified Parser.Require
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Task (Task)
import Utils.Files (findAllFilesIn)

type DependencyTree = Tree.Tree Require

type Dependencies = [DependencyTree]

{-| Find all dependencies for files in `module_directory`.
-}
find :: Config -> Task Dependencies
find config = do
  paths <- findAllFilesIn $ Config.module_directory config
  let modules = fmap (Ast.Require Ast.Js) paths
  let sourcePath = Config.source_directory config
  traverse (depsTree sourcePath) modules

depsTree :: FilePath -> Require -> Task DependencyTree
depsTree sourcePath = Tree.unfoldTreeM $ findRequires sourcePath

findRequires :: FilePath -> Require -> Task (Require, [Require])
findRequires sourcePath require =
  lift $ do
    let path = sourcePath </> Ast.fileName require
    exists <- doesFileExist path
    if not exists
      then return (require, [])
      else do
        content <- readFile $ path
        return (require, Parser.Require.jsRequires $ T.pack content)
