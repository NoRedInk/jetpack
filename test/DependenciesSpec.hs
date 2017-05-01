{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module DependenciesSpec where

import Config
import Control.Monad.State (modify)
import Data.List as L
import Data.Tree as Tree
import Dependencies
import DependencyTree
import Error
import Parser.Ast as Ast
import System.FilePath ((<.>), (</>))
import Task
import Test.Tasty
import Test.Tasty.HUnit

basicsFixtures :: Config
basicsFixtures =
  Config
    ("." </> "test" </> "fixtures" </> "basics" </> "modules")
    [("." </> "test" </> "fixtures" </> "basics" </> "node_modules")]
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
    []
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
        e <- runTask $ do
          modify (\env -> env { config = basicsFixtures })
          DependencyTree.build [] ("test" <.> "js")
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
              , "." </> "test" </> "fixtures" </> "basics" </>  "node_modules" </> "lodash" </> "index.js"
              )
            , ( Ast.Js
              , "." </> "lodash.dist.js"
              , "." </> "test" </> "fixtures" </> "basics" </>  "node_modules" </> "lodash" </> "." </> "lodash.dist.js"
              )
            , ( Ast.Js
              , "." </> "lodash"
              , "." </> "test" </> "fixtures" </> "basics" </> "node_modules" </> "lodash" </> "." </> "." </> "lodash.js"
              )
            , ( Ast.Js
              , "" </> "debug"
              , "." </> "test" </> "fixtures" </> "basics" </> "node_modules" </> "lodash" </> "." </> "node_modules" </> "debug.js"
              )
            ]
    , testCase "#find failing" $ do
        e <- runTask $ do
          modify (\env -> env { config = failingFixtures })
          DependencyTree.build [] ("test" <.> "js")
        case e of
          Right _ -> assertFailure $ "This shouldn't pass"
          Left errors ->
            L.last errors @?=
            ModuleNotFound
              (Config.entry_points failingFixtures)
              (Config.source_directory failingFixtures)
              "\"index\""
    ]

dropLastMod :: Dependency -> (Ast.SourceType, FilePath, FilePath)
dropLastMod Dependency { fileType, requiredAs, filePath } = (fileType, requiredAs, filePath)
