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
             { entry_points = "app" </> "modules"
             , modules_directories = []
             , source_directory = "app" </> "sources"
             , elm_root_directory = "app" </> "sources"
             , temp_directory = "app" </> "tmp"
             , log_directory = "app" </> "logs"
             , output_js_directory = "app" </> "js"
             , elm_make_path = Nothing
             , coffee_path = Nothing
             , no_parse =
                 ["." </> "node_modules" </> "clipboard" </> "clipboard.js"]
             , watch_file_extensions = [".elm", ".coffee", ".js", ".json"]
             })
    ]
