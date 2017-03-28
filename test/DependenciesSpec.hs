{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module DependenciesSpec where

import Config
import Control.Monad.Except (runExceptT)
import Data.List as L
import Data.Tree as Tree
import Dependencies
import DependencyTree
import Error
import Parser.Ast as Ast
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

basicsFixtures :: Config
basicsFixtures =
  Config
    ("." </> "test" </> "fixtures" </> "basics" </> "modules")
    ("." </> "test" </> "fixtures" </> "basics" </> "sources")
    ("." </> "test" </> "fixtures" </> "basics" </> "sources")
    []
    ("." </> "test" </> "fixtures" </> "basics" </> "tmp")
    ("." </> "test" </> "fixtures" </> "basics" </> "logs")
    ("." </> "test" </> "fixtures" </> "basics" </> "js")
    ("." </> "test" </> "fixtures" </> "basics" </> "css")
    Nothing
    Nothing
    Nothing

failingFixtures :: Config
failingFixtures =
  Config
    ("." </> "test" </> "fixtures" </> "failing" </> "modules")
    ("." </> "test" </> "fixtures" </> "failing" </> "sources")
    ("." </> "test" </> "fixtures" </> "failing" </> "sources")
    []
    ("." </> "test" </> "fixtures" </> "failing" </> "tmp")
    ("." </> "test" </> "fixtures" </> "failing" </> "logs")
    ("." </> "test" </> "fixtures" </> "failing" </> "js")
    ("." </> "test" </> "fixtures" </> "failing" </> "css")
    Nothing
    Nothing
    Nothing

suite :: TestTree
suite =
  testGroup
    "Dependencies"
    [ testCase "#find success" $ do
        e <- runExceptT $ do DependencyTree.build basicsFixtures Nothing
        case e of
          Left msg -> do
            _ <- traverse print msg
            assertFailure $ "This shouldn't fail"
          Right deps ->
            fmap (fmap dropLastMod . Tree.flatten) deps @?=
            [ [ ( Ast.Js
                , "" </> "test.js"
                , "." </> "test" </> "fixtures" </> "basics" </> "modules" </> "test.js"
                )
              , ( Ast.Coffee
                , "" </> "index"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> "index.coffee"
                )
              , ( Ast.Js
                , "" </> "lodash"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </> "index.js"
                )
              , ( Ast.Js
                , "." </> "lodash.dist.js"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </> "." </> "lodash.dist.js"
                )
              , ( Ast.Js
                , "." </> "lodash"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </> "." </> "." </> "lodash.js"
                )
              , ( Ast.Js
                , "" </> "debug"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </> "." </> "node_modules" </> "debug.js"
                )
              ]
            ]
    , testCase "#find failing" $ do
        e <- runExceptT $ do DependencyTree.build failingFixtures Nothing
        case e of
          Right _ -> assertFailure $ "This shouldn't fail"
          Left errors ->
            L.last errors @?=
            ModuleNotFound
              (Config.module_directory failingFixtures)
              (Config.source_directory failingFixtures)
              "\"index\""
    ]

dropLastMod :: Dependency -> (Ast.SourceType, FilePath, FilePath)
dropLastMod Dependency { fileType, requiredAs, filePath } = (fileType, requiredAs, filePath)
