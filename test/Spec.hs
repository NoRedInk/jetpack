import ConcatModuleSpec
import ConfigSpec
import DependenciesSpec
import Parser.CommentSpec
import Parser.RequireSpec
import System.FilePath.Glob (glob)
import Test.DocTest
import Test.Tasty

main :: IO ()
main = runDocTests >> runTests

runTests :: IO ()
runTests = do
  defaultMain $
    testGroup
      "jetpack"
      [ testGroup
          "suites"
          [ ConfigSpec.suite
          , ConcatModuleSpec.suite
          , DependenciesSpec.suite
          , Parser.RequireSpec.suite
          , Parser.CommentSpec.suite
          ]
      , testGroup
          "properties"
          [Parser.RequireSpec.properties, Parser.CommentSpec.properties]
      ]

runDocTests :: IO ()
runDocTests = glob "src/**/*.hs" >>= doctest
