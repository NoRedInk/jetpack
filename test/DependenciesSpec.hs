{-# LANGUAGE OverloadedStrings #-}

module DependenciesSpec where

import Config
import Control.Monad.Except (runExceptT)
import Data.List as L
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
            fmap (Tree.flatten) deps @?=
            [ [ Dependency Ast.Js ("" </> "test.js") $
                "." </> "test" </> "fixtures" </> "basics" </> "modules" </> "test.js"
              , Dependency Ast.Coffee ("" </> "index") $
                "." </> "test" </> "fixtures" </> "basics" </> "sources" </> "index.coffee"
              , Dependency Ast.Js ("" </> "lodash") $
                "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </>
                "index.js"
              , Dependency Ast.Js ("." </> "lodash.dist.js") $
                "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </>
                "." </>
                "lodash.dist.js"
              , Dependency Ast.Js ("." </> "lodash") $
                "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </>
                "." </>
                "." </>
                "lodash.js"
              , Dependency Ast.Js ("" </> "debug") $
                "." </> "test" </> "fixtures" </> "basics" </> "sources" </> ".." </> "node_modules" </> "lodash" </>
                "." </>
                "node_modules" </>
                "debug.js"
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
