module ConfigSpec where

import Config
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "Config"
    [ testCase "#load success" $ do
        config <- Config.load "./test/fixtures"
        config @=?
          (Config.Config
           { entryPoints = "app" </> "modules"
           , modulesDirs = []
           , sourceDir = "app" </> "sources"
           , elmRoot = "app" </> "sources"
           , tempDir = "app" </> "tmp"
           , logDir = "app" </> "logs"
           , outputDir = "app" </> "js"
           , elmPath = Nothing
           , coffeePath = Nothing
           , noParse =
               ["." </> "node_modules" </> "clipboard" </> "clipboard.js"]
           , watchFileExt = [".elm", ".coffee", ".js", ".json"]
           , watchIgnorePatterns = ["/[.]#[^/]*$", "/~[^/]*$"]
           })
    ]
