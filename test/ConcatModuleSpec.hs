{-# LANGUAGE OverloadedStrings #-}
module ConcatModuleSpec where

import ConcatModule
import Config
import Control.Monad.State (modify)
import Data.Foldable
import Data.Text as T
import Data.Tree as Tree
import Dependencies as D
import Parser.Ast as Ast
import System.Directory (removeFile)
import System.FilePath ((<.>), (</>))
import Task
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
  [ "/* START: testFunction */"
  , "function testFunction(module, exports) {"
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
    []
    ("." </> "test" </> "fixtures" </> "concat" </> "sources")
    ("." </> "test" </> "fixtures" </> "concat" </> "sources")
    []
    ("." </> "test" </> "fixtures" </> "concat" </> "tmp")
    ("." </> "test" </> "fixtures" </> "concat" </> "logs")
    ("." </> "test" </> "fixtures" </> "concat" </> "js")
    ("." </> "test" </> "fixtures" </> "concat" </> "css")
    Nothing
    Nothing
    Nothing

mockDependency :: FilePath -> FilePath -> D.Dependency
mockDependency f p = D.Dependency Ast.Js f p Nothing

mockDependencies :: D.Dependencies
mockDependencies =
  [Tree.Node (dependency "modules" "Foo")
    [ Tree.Node (dependency "sources" "Moo") []
    ]
  ]
  where dependency location fileName = mockDependency ("." </> fileName) ("." </> "test" </> "fixtures" </> "concat" </> location </> "Page" </> fileName <.> "js")

expectedOutput :: [String]
expectedOutput =
  [ T.unpack $ T.unlines
    [ "(function() {"
    , "var jetpackCache = {};"
    , "function jetpackRequire(fn) {"
    , "  var e = {};"
    , "  var m = { exports : e };"
    , "  if (typeof fn !== \"function\") {"
    , "    console.error(\"Required function isn't a jetpack module.\", fn)"
    , "    return;"
    , "  }"
    , "  if (jetpackCache[fn.name]) {"
    , "    return jetpackCache[fn.name];"
    , "  }"
    , "  jetpackCache[fn.name] = m.exports;"
    , "  fn(m, e);  "
    , "  jetpackCache[fn.name] = m.exports;"
    , "  return m.exports;"
    , "}"
    ,"/* START: test___fixtures___concat___modules___Page___Foo_js_js */"
    ,"function test___fixtures___concat___modules___Page___Foo_js_js(module, exports) {"
    ,"var moo = jetpackRequire(test___fixtures___concat___sources___Page___Moo_js_js);"
    ,"moo(4, 2);"
    ,"} /* END: test___fixtures___concat___modules___Page___Foo_js_js */"
    ,"/* START: test___fixtures___concat___sources___Page___Moo_js_js */"
    ,"function test___fixtures___concat___sources___Page___Moo_js_js(module, exports) {"
    ,"module.exports = function(a, b) {"
    ,"  console.log(a + b + \"\");"
    ,"};"
    ,"} /* END: test___fixtures___concat___sources___Page___Moo_js_js */"
    ,""
    ,"jetpackRequire(test___fixtures___concat___modules___Page___Foo_js_js);"
    ,"})();"
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
    , testCase "#replaceRequire replaces require(string) with jetpackRequire(function)" $ do
        replaceRequire (mockDependency "foo" $ "ui" </> "src" </> "foo") "var x = require('foo')"
        @?= "var x = jetpackRequire(ui___src___foo_js)"
    , testCase "#replaceRequire replaces require(string) with jetpackRequire(function)" $ do
        replaceRequire (mockDependency "foo" $ "ui" </> "src" </> "foo") "var x = require(\"foo\")"
        @?= "var x = jetpackRequire(ui___src___foo_js)"
    , testCase "#wrap" $ do
    e <- runTask $ do
      modify (\env -> env { config = mockConfig })
      traverse wrap mockDependencies
    case e of
      Left _  -> assertFailure ""
      Right paths -> do
        paths @?= ["./test/fixtures/concat/js/Page/Foo.js"]
        actual <- traverse readFile paths
        actual @?= expectedOutput
        traverse_ removeFile paths
    ]
