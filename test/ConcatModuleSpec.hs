{-# LANGUAGE OverloadedStrings #-}
module ConcatModuleSpec where

import ConcatModule
import Config
import Control.Monad.Except (runExceptT)
import Data.Foldable
import Data.Text as T
import Data.Tree as Tree
import Dependencies as D
import Parser.Ast as Ast
import System.Directory (removeFile)
import System.FilePath ((<.>), (</>))
import Test.Tasty
import Test.Tasty.HUnit


mockModule :: T.Text
mockModule =
  T.unlines
  [ "var foo = require('foo.js');"
  , ""
  , "foo(42)"
  ]

wrappedModule :: T.Text
wrappedModule =
  T.unlines
  [ "function testFunction(require, module, exports) {"
  , "var foo = require('foo.js');"
  , ""
  , "foo(42)"
  , "} /* END: testFunction */"
  ]

mockDependencyTree :: D.DependencyTree
mockDependencyTree =
  Tree.Node (dependency "index")
    [ Tree.Node (dependency "main") []
    , Tree.Node (dependency "index") []
    , Tree.Node (cssDependency "index") []
    , Tree.Node cssEntry []
    ]
  where
    dependency fileName = D.Dependency
                    Ast.Js
                    (fileName <.> "js")
                    ("ui" </> "src" </> fileName <.> "js")
                    Nothing
    cssEntry = D.Dependency
                    Ast.Js
                    ("foo" <.> "css")
                    ("ui" </> "src" </> "foo" <.> "css")
                    Nothing
    cssDependency fileName = D.Dependency
                    Ast.Sass
                    (fileName <.> "sass")
                    ("ui" </> "src" </> fileName <.> "sass")
                    Nothing

mockConfig :: Config
mockConfig =
  Config
    ("." </> "test" </> "fixtures" </> "concat" </> "modules")
    ("." </> "test" </> "fixtures" </> "concat" </> "sources")
    ("." </> "test" </> "fixtures" </> "concat" </> "sources")
    []
    ("." </> "test" </> "fixtures" </> "concat" </> "tmp")
    ("." </> "test" </> "fixtures" </> "concat" </> "logs")
    ("." </> "test" </> "fixtures" </> "concat" </> "js")
    ("." </> "test" </> "fixtures" </> "concat" </> "css")


mockDependencies :: D.Dependencies
mockDependencies =
  [Tree.Node (dependency "modules" "Foo")
    [ Tree.Node (dependency "sources" "Moo") []
    ]
  ]
  where
    dependency location fileName = D.Dependency
                    Ast.Js
                    (fileName <.> "js")
                    ("." </> "test" </> "fixtures" </> "concat" </> location</> "Page" </> fileName <.> "js")
                    Nothing

expectedOutput :: [String]
expectedOutput =
  [ T.unpack $ T.unlines
    [ "function test___fixtures___concat___modules___Page___Foo_js_js(require, module, exports) {"
    , "4 + 2"
    , "} /* END: test___fixtures___concat___modules___Page___Foo_js_js */"
    , "function test___fixtures___concat___sources___Page___Moo_js_js(require, module, exports) {"
    , "require('./foo')"
    , "console.log('foo')"
    , "} /* END: test___fixtures___concat___sources___Page___Moo_js_js */"
    ]
  ]

suite :: TestTree
suite =
  testGroup
    "ConcatModule"
    [ testCase "#wrapModule" $ do
        wrapModule "" "" @?= ""
    , testCase "#wrapModule wraps a module in a function" $ do
        wrapModule "testFunction" mockModule @?= wrappedModule
    , testCase "#wrap" $ do
        e <- runExceptT $ wrap mockConfig mockDependencies
        case e of
          Left _  -> assertFailure ""
          Right paths -> do
            paths @?= ["./test/fixtures/concat/js/Page/Foo.js"]
            actual <- traverse readFile paths
            actual @?= expectedOutput
            traverse_ removeFile paths
    ]
