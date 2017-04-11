{-# LANGUAGE OverloadedStrings #-}

module ConfigSpec where

import Config
import Error
import System.FilePath ((</>))
import Task
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "Config"
    [ testCase "#load success" $ do
        e <- runTask $ do Config.load "./test/fixtures"
        case e of
          Left _ -> assertFailure $ "Couldn't decode jetpack.json"
          Right config ->
            config @=?
            Just ( Config.Config
              ("app" </> "modules")
              ("app" </> "sources")
              ("app" </> "sources")
              []
              ("app" </> "tmp")
              ("app" </> "logs")
              ("app" </> "js")
              ("app" </> "css")
              Nothing
              Nothing
              Nothing
            )
    ]
