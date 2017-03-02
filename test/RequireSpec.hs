{-# LANGUAGE OverloadedStrings #-}

module RequireSpec where

import Control.Monad.Trans.Either (runEitherT)
import Data.Text as T
import qualified Require
import System.FilePath ((<.>), (</>))
import qualified Test.SmallCheck.Series.Text as T.Series
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

assertRequire :: T.Text -> Require.Require -> Assertion
assertRequire content require =
  case Require.require content of
    Just c -> c @?= require
    Nothing -> assertFailure "failed"

suite :: TestTree
suite =
  testGroup
    "Require"
    [ testCase "#require" $
      assertRequire "var x = require(\"x.elm\")" $
      Require.Require Require.Elm "x.elm"
    , testCase "#require" $
      assertRequire "let foo = require(\"foo.elm\")" $
      Require.Require Require.Elm "foo.elm"
    , testCase "#require" $
      assertRequire "require(\"foo.bar.elm\")" $
      Require.Require Require.Elm $ "foo" <.> "bar.elm"
    , testCase "#require" $
      assertRequire "require(\"foo.bar.coffee\")" $
      Require.Require Require.Coffee $ "foo" <.> "bar.coffee"
    , testCase "#require" $
      assertRequire "require(\"foo.bar.sass\")" $
      Require.Require Require.Sass $ "foo" <.> "bar.sass"
    , testCase "#require" $
      assertRequire "require(\"foo.bar\")" $
      Require.Require Require.Js $ "foo" <.> "bar"
    , testCase "#require" $
      assertRequire "require(\"foo.bar.js\")" $
      Require.Require Require.Js $ "foo" <.> "bar.js"
    , testCase "#require" $
      assertRequire "coffee = require \"foo.bar.js\" " $
      Require.Require Require.Js $ "foo" <.> "bar.js"
    , testCase "#require" $
      assertRequire "require 'foo.bar.js' " $
      Require.Require Require.Js $ "foo" <.> "bar.js"
    , testCase "#require node_module" $
      assertRequire "require 'lodash'" $ Require.Require Require.Js "lodash"
    , testCase "#require multilines" $
      assertRequire
        (T.unlines ["// test", "moo = require  'foo.bar.js'", "moo(42)"]) $
      Require.Require Require.Js $ "foo" <.> "bar.js"
    , testCase "#require multilines" $
      assertRequire
        (T.unlines ["// test", "var moo = require('foo.bar.js');", "moo(42)"]) $
      Require.Require Require.Js $ "foo" <.> "bar.js"
    -- TODO add failing tests
    ]

properties :: TestTree
properties =
  testGroup
    "Require Properties"
    [ testProperty "#require" $ \name ext ->
        (T.length (T.pack ext) > 0) ==>
        case require name (T.pack ext) of
          Just c ->
            c == (Require.Require (Require.getFileType ext) $ name <.> ext)
          Nothing -> False
    ]
  where
    require name ext =
      Require.require $ T.concat ["\nrequire '", T.pack name, ".", ext, "'\n"]
