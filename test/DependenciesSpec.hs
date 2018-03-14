module DependenciesSpec where

import Config
import Control.Monad.Except (runExceptT)
import Control.Monad.State (modify)
import Data.List as L
import Data.Tree as Tree
import Dependencies
import DependencyTree
import Error
import Parser.Ast as Ast
import System.Console.AsciiProgress
import System.FilePath ((<.>), (</>))
import Task
import Task (Task, lift)
import Test.Tasty
import Test.Tasty.HUnit

mockProgressBar :: IO ProgressBar
mockProgressBar =
  newProgressBar
    def
    { pgTotal = toInteger 1
    , pgOnCompletion = Just ""
    , pgCompletedChar = ' '
    , pgPendingChar = ' '
    , pgFormat = ""
    }

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
  , no_parse = []
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
  }

suite :: TestTree
suite =
  testGroup
    "Dependencies"
    [ testCase "#find success" $ do
        e <-
          runExceptT $ do
            pg <- lift mockProgressBar
            DependencyTree.build pg basicsFixtures [] ("test" <.> "js")
        case e of
          Left msg -> do
            _ <- traverse print msg
            assertFailure $ "This shouldn't fail"
          Right dep ->
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
    , testCase "#find failing" $ do
        e <-
          runExceptT $ do
            pg <- lift mockProgressBar
            DependencyTree.build pg failingFixtures [] ("test" <.> "js")
        case e of
          Right x -> assertFailure $ "This shouldn't pass"
          Left errors ->
            L.last errors @?=
            ModuleNotFound
              (Just "./test/fixtures/failing/modules/test.js")
              "\"index\""
    ]

dropLastMod :: Dependency -> (Ast.SourceType, FilePath, FilePath)
dropLastMod Dependency {fileType, requiredAs, filePath} =
  (fileType, requiredAs, filePath)
