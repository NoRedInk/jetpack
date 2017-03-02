import ConfigSpec
import RequireSpec
import System.FilePath.Glob (glob)
import Test.DocTest
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = runDocTests >> runTests

runTests :: IO ()
runTests = do
  defaultMain $
    testGroup
      "jetpack"
      [ testGroup "suites" [ConfigSpec.suite, RequireSpec.suite]
      , testGroup "properties" [RequireSpec.properties]
      ]

runDocTests :: IO ()
runDocTests = glob "src/**/*.hs" >>= doctest
