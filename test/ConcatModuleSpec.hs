{-# LANGUAGE OverloadedStrings #-}
module ConcatModuleSpec where

import ConcatModule
import Parser.Ast as Ast
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.HUnit

import Data.Text as T
import Data.Tree as Tree
import Dependencies as D


mockModule :: T.Text
mockModule =
  T.unlines
  [ "var foo = require('foo.js');"
  , ""
  , "foo(42)"
  ]

wrappedModule :: T.Text
wrappedModule =
  T.unlines
  [ "function(require, module, exports) {"
  , "var foo = require('foo.js');"
  , ""
  , "foo(42)"
  , "}"
  ]

mockDependencyTree :: D.DependencyTree
mockDependencyTree =
  Tree.Node (dependency "index")
    [ Tree.Node (dependency "main") []
    , Tree.Node (dependency "index") []
    , Tree.Node (cssDependency "index") []
    , Tree.Node cssEntry []
    ]
  where
    dependency fileName = D.Dependency
                    Ast.Js
                    (fileName <.> "js")
                    ("ui" </> "src" </> fileName <.> "js")
                    Nothing
    cssEntry = D.Dependency
                    Ast.Js
                    ("foo" <.> "css")
                    ("ui" </> "src" </> "foo" <.> "css")
                    Nothing
    cssDependency fileName = D.Dependency
                    Ast.Sass
                    (fileName <.> "sass")
                    ("ui" </> "src" </> fileName <.> "sass")
                    Nothing


suite :: TestTree
suite =
  testGroup
    "ConcatModule"
    [ testCase "#wrapModule" $ do
        wrapModule "" @?= ""
    , testCase "#wrapModule wraps a module in a function" $ do
        wrapModule mockModule @?= wrappedModule
    , testCase "#getCompiledDependencyFileNames" $ do
        getCompiledDependencyFileNames mockDependencyTree @?=
          [ Just "ui@@@src@@@index.js.js"
          , Just "ui@@@src@@@main.js.js"
          , Nothing
          , Nothing
          ]
    ]
