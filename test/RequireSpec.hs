{-# LANGUAGE OverloadedStrings #-}
module RequireSpec where


import Data.Text as T
import qualified Require
import Control.Monad.Trans.Either (runEitherT)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck
import qualified Test.SmallCheck.Series.Text as T.Series


assertRequire :: T.Text -> Require.Require -> Assertion
assertRequire content require =
  case Require.require content of
    Right c -> c @?= require
    Left msg -> assertFailure $ "failed: " ++ T.unpack msg


suite :: TestTree
suite = testGroup "Require"
    [ testCase "#require" $
        assertRequire "var x = require(\"x.elm\")"
          $ Require.Require "x" Require.Elm
    , testCase "#require" $
        assertRequire "let foo = require(\"foo.elm\")"
          $ Require.Require "foo" Require.Elm
    , testCase "#require" $
        assertRequire "require(\"foo.bar.elm\")"
          $ Require.Require "foo.bar" Require.Elm
    , testCase "#require" $
        assertRequire "require(\"foo.bar.coffee\")"
          $ Require.Require "foo.bar" Require.Coffee
    , testCase "#require" $
        assertRequire "require(\"foo.bar.sass\")"
          $ Require.Require "foo.bar" Require.Sass
    , testCase "#require" $
        assertRequire "require(\"foo.bar\")"
          $ Require.Require "foo.bar" Require.Js
    , testCase "#require" $
        assertRequire "require(\"foo.bar.js\")"
          $ Require.Require "foo.bar" Require.Js
    , testCase "#require" $
        assertRequire "coffee = require \"foo.bar.js\" "
          $ Require.Require "foo.bar" Require.Js
    , testCase "#require" $
        assertRequire "require 'foo.bar.js' "
          $ Require.Require "foo.bar" Require.Js
    , testCase "#require multilines" $
        assertRequire
          ( T.unlines
            [ "// test"
            , "moo = require 'foo.bar.js'"
            , "moo(42)"
            ]
          )
          $ Require.Require "foo.bar" Require.Js
    , testCase "#require multilines" $
        assertRequire
          ( T.unlines
            [ "// test"
            , "var moo = require('foo.bar.js');"
            , "moo(42)"
            ]
          )
          $ Require.Require "foo.bar" Require.Js
    ]

properties :: TestTree
properties = testGroup "Require Properties"
  [ testProperty "#require" $
      \name ->
        case Require.require (T.concat ["require '", T.pack name, ".js' "]) of
          Right c -> c == Require.Require (T.pack name) Require.Js
          Left msg -> False
  ]
