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
import System.Console.AsciiProgress
import System.FilePath ((<.>), (</>))
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
  { entryPoints =
      EntryPoints ("." </> "test" </> "fixtures" </> "basics" </> "modules")
  , modulesDirs =
      [ ModulesDir
          ("." </> "test" </> "fixtures" </> "basics" </> "node_modules")
      ]
  , sourceDir =
      SourceDir ("." </> "test" </> "fixtures" </> "basics" </> "sources")
  , elmRoot = ElmRoot ("." </> "test" </> "fixtures" </> "basics" </> "sources")
  , tempDir = ("." </> "test" </> "fixtures" </> "basics" </> "tmp")
  , logDir = ("." </> "test" </> "fixtures" </> "basics" </> "logs")
  , outputDir = ("." </> "test" </> "fixtures" </> "basics" </> "js")
  , elmPath = Nothing
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
  { entryPoints =
      EntryPoints ("." </> "test" </> "fixtures" </> "failing" </> "modules")
  , modulesDirs = []
  , sourceDir =
      SourceDir ("." </> "test" </> "fixtures" </> "failing" </> "sources")
  , elmRoot =
      ElmRoot ("." </> "test" </> "fixtures" </> "failing" </> "sources")
  , tempDir = ("." </> "test" </> "fixtures" </> "failing" </> "tmp")
  , logDir = ("." </> "test" </> "fixtures" </> "failing" </> "logs")
  , outputDir = ("." </> "test" </> "fixtures" </> "failing" </> "js")
  , elmPath = Nothing
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
        pg <- mockProgressBar
        dep <- DependencyTree.build pg basicsFixtures [] ("test" <.> "js")
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
        pg <- mockProgressBar
        dep <-
          DependencyTree.build pg basicsFixtures [] ("test_no_parse" <.> "js")
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
        pg <- mockProgressBar
        result <-
          ES.tryAny $
          DependencyTree.build pg failingFixtures [] ("test" <.> "js")
        case result of
          Left err ->
            show err @?=
            "\n\nI had troubles finding 'index' required in './test/fixtures/failing/modules/test.js'.\n\nMake sure that you spelled the name of the module correctly.\nYou might also want to make sure that all dependencies are updated.\n"
          Right _ -> assertFailure $ "This shouldn't pass"
    ]

dropLastMod :: Dependency -> (Ast.SourceType, FilePath, FilePath)
dropLastMod Dependency {fileType, requiredAs, filePath} =
  (fileType, requiredAs, filePath)
