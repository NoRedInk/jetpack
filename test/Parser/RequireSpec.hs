{-# LANGUAGE OverloadedStrings #-}

module Parser.RequireSpec where

import Control.Monad.Trans.Either (runEitherT)
import Data.Maybe as M
import Data.Text as T
import Helper.Property
import qualified Parser.Require as Require
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

assertRequire :: T.Text -> Require.Require -> Assertion
assertRequire content require =
  case Require.require content of
    Just c -> c @?= require
    Nothing -> assertFailure "failed"

assertParsingFails :: T.Text -> Assertion
assertParsingFails =
  assertBool "unexpected success" . M.isNothing . Require.require

suite :: TestTree
suite =
  testGroup
    "Require"
    [ testCase ".elm" $
      assertRequire "var x = require(\"x.elm\")" $
      Require.Require Require.Elm "x.elm"
    , testCase ".elm with namespace" $
      assertRequire "let foo = require(\"foo.elm\")" $
      Require.Require Require.Elm "foo.elm"
    , testCase ".elm" $
      assertRequire "require(\"foo.bar.elm\")" $
      Require.Require Require.Elm $ "foo" <.> "bar.elm"
    , testCase ".coffee" $
      assertRequire "require(\"foo.bar.coffee\")" $
      Require.Require Require.Coffee $ "foo" <.> "bar.coffee"
    , testCase "sass" $
      assertRequire "require(\"foo.bar.sass\")" $
      Require.Require Require.Sass $ "foo" <.> "bar.sass"
    , testCase "no ext" $
      assertRequire "require(\"foo.bar\")" $
      Require.Require Require.Js $ "foo" <.> "bar"
    , testCase "js" $
      assertRequire "require(\"foo.bar.js\")" $
      Require.Require Require.Js $ "foo" <.> "bar.js"
    , testCase "coffee" $
      assertRequire "coffee = require \"foo.bar.js\" " $
      Require.Require Require.Js $ "foo" <.> "bar.js"
    , testCase "js" $
      assertRequire "require 'foo.bar.js' " $
      Require.Require Require.Js $ "foo" <.> "bar.js"
    , testCase "node_module" $
      assertRequire "require 'lodash'" $ Require.Require Require.Js "lodash"
    , testCase "multilines" $
      assertRequire
        (T.unlines ["// test", "moo = require  'foo.bar.js'", "moo(42)"]) $
      Require.Require Require.Js $ "foo" <.> "bar.js"
    , testCase "multilines" $
      assertRequire
        (T.unlines ["// test", "var moo = require('foo.bar.js');", "moo(42)"]) $
      Require.Require Require.Js $ "foo" <.> "bar.js"
    , testCase "fails" $
      assertParsingFails $
      T.unlines ["// test", "var moo = require('foo.bar.js';", "moo(42)"]
    ]

properties :: TestTree
properties =
  testGroup
    "Require Properties"
    [ testProperty "#require" $ \(AlphaNum name) (AlphaNum ext) ->
        case require name ext of
          Just c ->
            c ==
            (Require.Require (Require.getFileType $ T.unpack ext) $
             T.unpack name <.> T.unpack ext)
          Nothing -> False
    ]
  where
    require name ext =
      Require.require $ T.concat ["\nrequire '", name, ".", ext, "'\n"]
