{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec where

import Config
import Control.Monad.Trans.Either (runEitherT)
import Error
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "Config"
    [ testCase "#load success" $ do
        e <- runEitherT $ do Config.load "./test/fixtures"
        case e of
          Left _ -> assertFailure $ "Couldn't decode jetpack.json"
          Right config ->
            config @=?
            Config.Config
              ("app" </> "modules")
              ("app" </> "sources")
              ("app" </> "tmp")
              ("app" </> "js")
              ("app" </> "css")
    , testCase "#load failure" $ do
        e <- runEitherT $ do Config.load "./test"
        case e of
          Left msg ->
            fmap Error.description msg @=?
            ["Couldn't find file: \"./test/jetpack.json\""]
          Right config -> assertFailure $ "This shouldn't succeed"
    ]
