{-# LANGUAGE OverloadedStrings #-}

module Parser.CommentSpec where

import Control.Monad.Trans.Either (runEitherT)
import Data.Maybe as M
import Data.Text as T
import Helper.Property
import qualified Parser.Comment
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

suite :: TestTree
suite =
  testGroup
    "Parser.Comment"
    [ testCase "parse no comments" $
      Parser.Comment.eatJsComments "asdf" @?= "asdf"
    , testCase "parse block comments" $
      Parser.Comment.eatJsComments "moo/*foo*/boo" @?= "mooboo"
    , testCase "parse block comments starting at the beginning" $
      Parser.Comment.eatJsComments "/*foo*/boo" @?= "boo"
    , testCase "parse block comments" $
      "a\n\nb  c\n\n" @=?
      (Parser.Comment.eatJsComments $
       T.unlines ["a\n", "b /* ignore", "xxxxxx", "  */ c\n"])
    , testCase "parse line comments" $
      "a\n\nb  c\n\n" @=?
      (Parser.Comment.eatJsComments $
       T.unlines ["a\n", "b // ignore", "//xxxxxx", " c\n"])
    , testCase "parse block and line comments" $
      "a\n\nb  c\n\nb \nOOOO\noooo\n\n" @=?
      (Parser.Comment.eatJsComments $
       T.unlines
         [ "a\n"
         , "b // ignore"
         , "//xxxxxx"
         , " c\n"
         , "b /* BLOCK"
         , "BLOCK"
         , "*/"
         , "OOOO"
         , "// ignore"
         , "oooo"
         , "/* IGNORE"
         , "IGNORE"
         , "*/"
         ])
    , testCase "only comments" $
      "" @=? (Parser.Comment.eatJsComments $ T.concat ["// foo", "/* asdf */"])
    , testCase "only comments" $
      "" @=? (Parser.Comment.eatJsComments $ T.concat ["/* foo */", "// asdf"])
    , testCase "only comments" $
      "" @=? (Parser.Comment.eatJsComments $ T.concat ["// foo", "/* asdf */"])
    ]

properties :: TestTree
properties =
  testGroup
    "Parser.Comment Properties"
    [ testProperty "#eatJsComments" $ \b1 b2 b3 ->
        (Parser.Comment.eatJsComments $
         Parser.Comment.eatJsComments $ codeWithJsComments b1 b2 b3) ==
        (Parser.Comment.eatJsComments $ codeWithJsComments b1 b2 b3)
    , testProperty "#eatJsComments" $ \b1 b2 b3 ->
        (Parser.Comment.eatJsComments $ codeWithJsComments b1 b2 b3) /=
        (codeWithJsComments b1 b2 b3)
    , testProperty "#eatCoffeeComments" $ \b1 b2 b3 ->
        (Parser.Comment.eatCoffeeComments $
         Parser.Comment.eatCoffeeComments $ codeWithCoffeeComments b1 b2 b3) ==
        (Parser.Comment.eatCoffeeComments $ codeWithCoffeeComments b1 b2 b3)
    , testProperty "#eatCoffeeComments" $ \b1 b2 b3 ->
        (Parser.Comment.eatCoffeeComments $ codeWithCoffeeComments b1 b2 b3) /=
        (codeWithCoffeeComments b1 b2 b3)
    ]
  where
    codeWithJsComments (CodeNoComments b1) (CodeNoComments b2) (CodeNoComments b3) =
      T.concat [b1, "// hello", b2, "/* \nworld\n */", b3]
    codeWithCoffeeComments (CodeNoComments b1) (CodeNoComments b2) (CodeNoComments b3) =
      T.concat [b1, "# hello", b2, "\n###\nworld\n###\n", b3]
