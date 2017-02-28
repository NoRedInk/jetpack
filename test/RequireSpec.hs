{-# LANGUAGE OverloadedStrings #-}
module RequireSpec where


import Data.Text as T
import qualified Require
import Control.Monad.Trans.Either (runEitherT)
import Test.Tasty
import Test.Tasty.HUnit


assertRequire :: T.Text -> Require.Require -> Assertion
assertRequire content require =
  case Require.require content of
    Right c -> c @?= require
    Left msg -> assertFailure $ "failed: " ++ T.unpack msg

suite :: TestTree
suite = testGroup "Require"
    [ testCase "#require" $
        assertRequire "require(\"x.elm\")"
          $ Require.Require "x" Require.Elm
    , testCase "#require" $
        assertRequire "require(\"foo.elm\")"
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
        assertRequire "require \"foo.bar.js\" "
          $ Require.Require "foo.bar" Require.Js
    , testCase "#require" $
        assertRequire "require 'foo.bar.js' "
          $ Require.Require "foo.bar" Require.Js
    ]
