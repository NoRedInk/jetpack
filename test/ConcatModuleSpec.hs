{-# LANGUAGE OverloadedStrings #-}
module ConcatModuleSpec where

import Parser.Ast as Ast
import ConcatModule
import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath ((<.>), (</>))

import Dependencies as D
import Data.Text as T
import Data.Tree as Tree


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
  Tree.Node (dependency "index") [Tree.Node (dependency "main") [], Tree.Node (dependency "index") []]
  where
    dependency fileName = D.Dependency
                    Ast.Js
                    (fileName <.> "js")
                    ("ui" </> "src" </> fileName <.> "js")
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
        getCompiledDependencyFileNames mockDependencyTree @?= ["ui@@@src@@@index.js.js", "ui@@@src@@@main.js.js"]
    ]
