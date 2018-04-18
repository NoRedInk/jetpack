module DependenciesSpec where

import Config
import qualified Control.Exception.Safe as ES
import Control.Monad.Except (runExceptT)
import Control.Monad.State (modify)
import Data.List as L
import Data.Tree as Tree
import Dependencies
import DependencyTree
import Parser.Ast as Ast
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.HUnit

basicsFixtures :: Config
basicsFixtures =
  Config
  { entry_points = ("." </> "test" </> "fixtures" </> "basics" </> "modules")
  , modules_directories =
      [("." </> "test" </> "fixtures" </> "basics" </> "node_modules")]
  , source_directory =
      ("." </> "test" </> "fixtures" </> "basics" </> "sources")
  , elm_root_directory =
      ("." </> "test" </> "fixtures" </> "basics" </> "sources")
  , temp_directory = ("." </> "test" </> "fixtures" </> "basics" </> "tmp")
  , log_directory = ("." </> "test" </> "fixtures" </> "basics" </> "logs")
  , output_js_directory = ("." </> "test" </> "fixtures" </> "basics" </> "js")
  , elm_make_path = Nothing
  , coffee_path = Nothing
  , no_parse =
      [ "." </> "test" </> "fixtures" </> "basics" </> "node_modules" </>
        "clipboard" </>
        "index.js"
      ]
  , watch_file_extensions = []
  , watch_file_ignore_patterns = []
  }

failingFixtures :: Config
failingFixtures =
  Config
  { entry_points = ("." </> "test" </> "fixtures" </> "failing" </> "modules")
  , modules_directories = []
  , source_directory =
      ("." </> "test" </> "fixtures" </> "failing" </> "sources")
  , elm_root_directory =
      ("." </> "test" </> "fixtures" </> "failing" </> "sources")
  , temp_directory = ("." </> "test" </> "fixtures" </> "failing" </> "tmp")
  , log_directory = ("." </> "test" </> "fixtures" </> "failing" </> "logs")
  , output_js_directory = ("." </> "test" </> "fixtures" </> "failing" </> "js")
  , elm_make_path = Nothing
  , coffee_path = Nothing
  , no_parse = []
  , watch_file_extensions = []
  , watch_file_ignore_patterns = []
  }

suite :: TestTree
suite =
  testGroup
    "Dependencies"
    [ testCase "#build success" $ do
        dep <- DependencyTree.build basicsFixtures [] ("test" <.> "js")
        (fmap dropLastMod $ Tree.flatten dep) @?=
          [ ( Ast.Js
            , "" </> "test.js"
            , "." </> "test" </> "fixtures" </> "basics" </> "modules" </>
              "test.js")
          , ( Ast.Coffee
            , "" </> "index"
            , "." </> "test" </> "fixtures" </> "basics" </> "sources" </>
              "index.coffee")
          , ( Ast.Js
            , "" </> "lodash"
            , "." </> "test" </> "fixtures" </> "basics" </> "node_modules" </>
              "lodash" </>
              "index.js")
          , ( Ast.Js
            , "." </> "lodash.dist.js"
            , "." </> "test" </> "fixtures" </> "basics" </> "node_modules" </>
              "lodash" </>
              "." </>
              "lodash.dist.js")
          , ( Ast.Js
            , "." </> "lodash"
            , "." </> "test" </> "fixtures" </> "basics" </> "node_modules" </>
              "lodash" </>
              "." </>
              "." </>
              "lodash.js")
          , ( Ast.Js
            , "" </> "debug"
            , "." </> "test" </> "fixtures" </> "basics" </> "node_modules" </>
              "lodash" </>
              "." </>
              "node_modules" </>
              "debug.js")
          ]
    , testCase "#build no_parse" $ do
        dep <- DependencyTree.build basicsFixtures [] ("test_no_parse" <.> "js")
        (fmap dropLastMod $ Tree.flatten dep) @?=
          [ ( Ast.Js
            , "" </> "test_no_parse.js"
            , "." </> "test" </> "fixtures" </> "basics" </> "modules" </>
              "test_no_parse.js")
          , ( Ast.Js
            , "" </> "no_parse_index"
            , "." </> "test" </> "fixtures" </> "basics" </> "sources" </>
              "no_parse_index.js")
          , ( Ast.Js
            , "clipboard"
            , "." </> "test" </> "fixtures" </> "basics" </> "node_modules" </>
              "clipboard" </>
              "index.js")
          ]
    , testCase "#build failing" $ do
        result <-
          ES.tryAny $ DependencyTree.build failingFixtures [] ("test" <.> "js")
        case result of
          Left err ->
            show err @?=
            "\n\nI had troubles finding 'index' required in './test/fixtures/failing/modules/test.js'.\n\nMake sure that you spelled the name of the module correctly.\nYou might also want to make sure that all dependencies are updated.\n"
          Right _ -> assertFailure $ "This shouldn't pass"
    ]

dropLastMod :: Dependency -> (Ast.SourceType, FilePath, FilePath)
dropLastMod Dependency {fileType, requiredAs, filePath} =
  (fileType, requiredAs, filePath)
