{-# LANGUAGE OverloadedStrings #-}

module DependenciesSpec where

import Config
import Control.Monad.Trans.Either (runEitherT)
import Data.List as L
import Data.Tree as Tree
import Dependencies
import Error
import Parser.Ast as Ast
import System.FilePath ((</>), joinPath, splitPath)
import Test.Tasty
import Test.Tasty.HUnit

basicsFixtures :: Config
basicsFixtures =
  Config
    ("." </> "test" </> "fixtures" </> "basics" </> "modules")
    ("." </> "test" </> "fixtures" </> "basics" </> "sources")
    ("." </> "test" </> "fixtures" </> "basics" </> "tmp")
    ("." </> "test" </> "fixtures" </> "basics" </> "js")
    ("." </> "test" </> "fixtures" </> "basics" </> "css")

failingFixtures :: Config
failingFixtures =
  Config
    ("." </> "test" </> "fixtures" </> "failing" </> "modules")
    ("." </> "test" </> "fixtures" </> "failing" </> "sources")
    ("." </> "test" </> "fixtures" </> "failing" </> "tmp")
    ("." </> "test" </> "fixtures" </> "failing" </> "js")
    ("." </> "test" </> "fixtures" </> "failing" </> "css")

suite :: TestTree
suite =
  testGroup
    "Dependencies"
    [ testCase "#find success" $ do
        e <- runEitherT $ do Dependencies.find basicsFixtures
        case e of
          Left msg -> assertFailure $ "This shouldn't fail"
          Right deps ->
            fmap (fmap pathsFromFixturesDir . Tree.flatten) deps @?=
            [ [ Dependency Ast.Js ("basics" </> "modules" </> "test.js") $
                "basics" </> "modules" </> "test.js"
              , Dependency Ast.Js ("" </> "index") $
                "basics" </> "sources" </> "index.js"
              , Dependency Ast.Js ("lodash" </> "lodash.dist.js") $
                "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </>
                "lodash.dist.js"
              ]
            ]
    , testCase "#find failing" $ do
        e <- runEitherT $ do Dependencies.find failingFixtures
        case e of
          Right _ -> assertFailure $ "This shouldn't fail"
          Left errors ->
            L.last errors @?=
            ModuleNotFound
              (Config.module_directory failingFixtures)
              (Config.source_directory failingFixtures)
              "\"index\""
    ]

pathsFromFixturesDir :: Dependency -> Dependency
pathsFromFixturesDir (Dependency t r p) =
  Dependency t (dropUntilFixtures r) (dropUntilFixtures p)
  where
    dropUntilFixtures path =
      if L.elem "fixtures/" $ splitPath path
        then joinPath $ tail $ L.dropWhile ((/=) "fixtures/") $ splitPath path
        else path
