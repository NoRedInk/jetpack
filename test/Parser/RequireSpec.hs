{-# LANGUAGE OverloadedStrings #-}

module Parser.RequireSpec where

import Control.Monad.Except (runExceptT)
import Data.Maybe as M
import Data.Text as T
import Helper.Property
import Parser.Ast as Ast
import qualified Parser.Require as Require
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

assertRequire :: T.Text -> [ Ast.Require ] -> Assertion
assertRequire content requires =
  case Require.require content of
    [] -> assertFailure "failed"
    rs -> rs @?= requires

assertParsingFails :: T.Text -> Assertion
assertParsingFails =
  assertBool "unexpected success" . (==) [] . Require.require

suite :: TestTree
suite =
  testGroup
    "Require"
    [ testCase ".elm" $
      assertRequire "var x = require(\"x.elm\")" [ Ast.Require Ast.Elm "x.elm" ]
    , testCase ".elm with namespace" $
      assertRequire "let foo = require(\"foo.elm\")"
        [ Ast.Require Ast.Elm "foo.elm" ]
    , testCase ".elm" $
      assertRequire "require(\"foo.bar.elm\");"
        [ Ast.Require Ast.Elm $ "foo" <.> "bar.elm" ]
    , testCase ".coffee" $
      assertRequire "require(\"foo.bar.coffee\")"
        [ Ast.Require Ast.Coffee $ "foo" <.> "bar.coffee" ]
    , testCase "sass" $
      assertRequire "require(\"foo.bar.sass\")"
        [ Ast.Require Ast.Sass $ "foo" <.> "bar.sass" ]
    , testCase "no ext" $
      assertRequire "require(\"foo.bar\")"
        [ Ast.Require Ast.Js $ "foo" <.> "bar" ]
    , testCase "js" $
      assertRequire "require(\"foo.bar.js\")"
        [ Ast.Require Ast.Js $ "foo" <.> "bar.js" ]
    , testCase "js with ;" $
      assertRequire "require(\"foo.bar.js\");"
        [ Ast.Require Ast.Js $ "foo" <.> "bar.js" ]
    , testCase "js" $
      assertRequire "require(\"foo.bar.js\"), require(\"moo.bar.js\")"
        [ Ast.Require Ast.Js $ "foo" <.> "bar.js"
        , Ast.Require Ast.Js $ "moo" <.> "bar.js"
        ]
    , testCase "coffee" $
      assertRequire "coffee = require \"foo.bar.js\""
        [ Ast.Require Ast.Js $ "foo" <.> "bar.js" ]
    , testCase "js" $
      assertRequire "require 'foo.bar.js' "
        [ Ast.Require Ast.Js $ "foo" <.> "bar.js" ]
    , testCase "node_module" $
      assertRequire "require 'lodash'" [ Ast.Require Ast.Js "lodash" ]
    , testCase "multilines" $
      assertRequire
        (T.unlines ["// test", "moo = require  'foo.bar.js'", "moo(42)"])
          [ Ast.Require Ast.Js $ "foo" <.> "bar.js" ]
    , testCase "multilines" $
      assertRequire
        (T.unlines ["// test", "var moo = require('foo.bar.js');", "moo(42)"])
          [ Ast.Require Ast.Js $ "foo" <.> "bar.js" ]
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
          [] -> False
          c ->
            c ==
              [ (Ast.Require (Require.getFileType $ T.unpack ext) $
                  T.unpack name <.> T.unpack ext)
              ]
    ]
  where
    require name ext =
      Require.require $ T.concat ["\nrequire '", name, ".", ext, "'\n"]
