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
        config <- Config.load "./test/fixtures"
        config @=?
          Just
            (Config.Config
               ("app" </> "modules")
               []
               ("app" </> "sources")
               ("app" </> "sources")
               ("app" </> "tmp")
               ("app" </> "logs")
               ("app" </> "js")
               Nothing
               Nothing)
    ]
