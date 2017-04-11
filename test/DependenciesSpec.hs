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
import System.Console.AsciiProgress
import System.FilePath ((<.>), (</>))
import Task
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
        pg <- newProgressBar def { pgWidth = 100 }
        e <- runTask $ do DependencyTree.build pg basicsFixtures [] ("test" <.> "js")
        case e of
          Left msg -> do
            _ <- traverse print msg
            assertFailure $ "This shouldn't fail"
          Right dep ->
            (fmap dropLastMod $ Tree.flatten dep) @?=
            [ ( Ast.Js
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
    , testCase "#find failing" $ do
        pg <- newProgressBar def { pgWidth = 100 }
        e <- runTask $ do DependencyTree.build pg failingFixtures [] ("test" <.> "js")
        case e of
          Right x -> assertFailure $ "This shouldn't pass"
          Left errors ->
            L.last errors @?=
            ModuleNotFound
              (Config.module_directory failingFixtures)
              (Config.source_directory failingFixtures)
              "\"index\""
    ]

dropLastMod :: Dependency -> (Ast.SourceType, FilePath, FilePath)
dropLastMod Dependency { fileType, requiredAs, filePath } = (fileType, requiredAs, filePath)
