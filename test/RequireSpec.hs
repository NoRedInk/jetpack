{-# LANGUAGE OverloadedStrings #-}

module RequireSpec where

import Control.Monad.Trans.Either (runEitherT)
import Data.Text as T
import qualified Require
import qualified Test.SmallCheck.Series.Text as T.Series
import System.FilePath ((<.>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

assertRequire :: T.Text -> Require.Require -> Assertion
assertRequire content require =
  case Require.require content of
    Right c -> c @?= require
    Left msg -> assertFailure $ "failed: " ++ T.unpack msg

suite :: TestTree
suite =
  testGroup
    "Require"
    [ testCase "#require" $
      assertRequire "var x = require(\"x.elm\")" $ Require.Require Require.Elm "x"
    , testCase "#require" $
      assertRequire "let foo = require(\"foo.elm\")" $ Require.Require Require.Elm "foo"
    , testCase "#require" $
      assertRequire "require(\"foo.bar.elm\")" $ Require.Require Require.Elm $ "foo" <.> "bar"
    , testCase "#require" $
      assertRequire "require(\"foo.bar.coffee\")" $ Require.Require Require.Coffee $ "foo" <.> "bar"
    , testCase "#require" $
      assertRequire "require(\"foo.bar.sass\")" $ Require.Require Require.Sass $ "foo" <.> "bar"
    , testCase "#require" $
      assertRequire "require(\"foo.bar\")" $ Require.Require Require.Js $ "foo" <.> "bar"
    , testCase "#require" $
      assertRequire "require(\"foo.bar.js\")" $ Require.Require Require.Js $ "foo" <.> "bar"
    , testCase "#require" $
      assertRequire "coffee = require \"foo.bar.js\" " $ Require.Require Require.Js $ "foo" <.> "bar"
    , testCase "#require" $
      assertRequire "require 'foo.bar.js' " $ Require.Require Require.Js $ "foo" <.> "bar"
    , testCase "#require multilines" $
      assertRequire (T.unlines ["// test", "moo = require 'foo.bar.js'", "moo(42)"]) $
        Require.Require Require.Js $ "foo" <.> "bar"
    , testCase "#require multilines" $
      assertRequire (T.unlines ["// test", "var moo = require('foo.bar.js');", "moo(42)"]) $
        Require.Require Require.Js $ "foo" <.> "bar"
    ]

properties :: TestTree
properties =
  testGroup
    "Require Properties"
    [ testProperty "#require" $ \name ->
        case Require.require (T.concat ["require '", T.pack name, ".js' "]) of
          Right c -> c == Require.Require Require.Js name
          Left msg -> False
    ]
