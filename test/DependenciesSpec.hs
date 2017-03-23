{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module DependenciesSpec where

import Config
import Control.Monad.Except (runExceptT)
import Data.List as L
import Data.Text as T
import Data.Tree as Tree
import Dependencies
import Error
import Parser.Ast as Ast
import System.FilePath (joinPath, splitPath, (</>))
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

suite :: TestTree
suite =
  testGroup
    "Dependencies"
    [ testCase "#find success" $ do
        e <- runExceptT $ do Dependencies.find basicsFixtures
        case e of
          Left msg -> do
            _ <- traverse print msg
            assertFailure $ "This shouldn't fail"
          Right deps ->
            fmap (fmap dropLastMod . Tree.flatten) deps @?=
              [ [ ( EntryPoint Ast.Js
                , T.pack "test.js"
                , "." </> "test" </> "fixtures" </> "basics" </> "modules" </> "test.js"
                )
              , ( EntryPoint Ast.Coffee
                , T.pack "index"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> "index.coffee"
                )
              , ( EntryPoint Ast.Js
                , T.pack "lodash"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </> "index.js"
                )
              , ( EntryPoint Ast.Js
                , T.pack "./lodash.dist.js"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </> "." </> "lodash.dist.js"
                )
              , ( EntryPoint Ast.Js
                , T.pack "./lodash"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </> "." </> "." </> "lodash.js"
                )
              , ( EntryPoint Ast.Js
                , T.pack "debug"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </> "." </> "node_modules" </> "debug.js"
                )
              , ( EntryPoint Ast.Elm
                , T.pack "Main.elm"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> "Main.elm"
                )
              , ( Extern Ast.Elm
                , T.pack "List"
                , "." </> "List"
                )
              , ( Child Ast.Elm
                , T.pack "Foo"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> "Foo.elm"
                )
              , ( Child Ast.Elm
                , T.pack "Page.Moo"
                , "." </> "test" </> "fixtures" </> "basics" </> "sources" </> "Page" </> "Moo.elm"
                )
              , ( Extern Ast.Elm
                , T.pack "Dict"
                , "." </> "Dict"
                )
              ]
            ]
    , testCase "#find failing" $ do
        e <- runExceptT $ do Dependencies.find failingFixtures
        case e of
          Right _ -> assertFailure $ "This shouldn't fail"
          Left errors ->
            L.last errors @?=
            ModuleNotFound
              (Config.module_directory failingFixtures)
              (Config.source_directory failingFixtures)
              "\"index\""
    ]

dropLastMod :: Dependency -> (FileType, T.Text, FilePath)
dropLastMod Dependency { fileType, requiredAs, filePath } = (fileType, requiredAs, filePath)
