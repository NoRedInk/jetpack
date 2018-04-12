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
  { entryPoints = ("." </> "test" </> "fixtures" </> "basics" </> "modules")
  , modulesDirs =
      [("." </> "test" </> "fixtures" </> "basics" </> "node_modules")]
  , sourceDir =
      ("." </> "test" </> "fixtures" </> "basics" </> "sources")
  , elmRoot =
      ("." </> "test" </> "fixtures" </> "basics" </> "sources")
  , tempDir = ("." </> "test" </> "fixtures" </> "basics" </> "tmp")
  , logDir = ("." </> "test" </> "fixtures" </> "basics" </> "logs")
  , outputDir = ("." </> "test" </> "fixtures" </> "basics" </> "js")
  , elmMakePath = Nothing
  , coffeePath = Nothing
  , noParse =
      [ "." </> "test" </> "fixtures" </> "basics" </> "node_modules" </>
        "clipboard" </>
        "index.js"
      ]
  , watchFileExt = []
  , watchIgnorePatterns = []
  }

failingFixtures :: Config
failingFixtures =
  Config
  { entryPoints = ("." </> "test" </> "fixtures" </> "failing" </> "modules")
  , modulesDirs = []
  , sourceDir =
      ("." </> "test" </> "fixtures" </> "failing" </> "sources")
  , elmRoot =
      ("." </> "test" </> "fixtures" </> "failing" </> "sources")
  , tempDir = ("." </> "test" </> "fixtures" </> "failing" </> "tmp")
  , logDir = ("." </> "test" </> "fixtures" </> "failing" </> "logs")
  , outputDir = ("." </> "test" </> "fixtures" </> "failing" </> "js")
  , elmMakePath = Nothing
  , coffeePath = Nothing
  , noParse = []
  , watchFileExt = []
  , watchIgnorePatterns = []
  }

suite :: TestTree
suite =
  testGroup
    "Dependencies"
    [ testCase "#build success" $ do
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
    , testCase "#build no_parse" $ do
        e <-
          runExceptT $ do
            pg <- lift mockProgressBar
            DependencyTree.build pg basicsFixtures [] ("test_no_parse" <.> "js")
        case e of
          Left msg -> do
            _ <- traverse print msg
            assertFailure $ "This shouldn't fail"
          Right dep ->
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
              "index"
    ]

dropLastMod :: Dependency -> (Ast.SourceType, FilePath, FilePath)
dropLastMod Dependency {fileType, requiredAs, filePath} =
  (fileType, requiredAs, filePath)
