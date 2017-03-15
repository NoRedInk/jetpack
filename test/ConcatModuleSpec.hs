{-# LANGUAGE OverloadedStrings #-}
module ConcatModuleSpec where

import ConcatModule
import Test.Tasty
import Test.Tasty.HUnit

import Data.Text as T

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

suite :: TestTree
suite =
  testGroup
    "ConcatModule"
    [ testCase "#wrapModule" $ do
        wrapModule "" @?= ""
    , testCase "#wrapModule wraps a module in a function" $ do
        wrapModule mockModule @?= wrappedModule
    ]
