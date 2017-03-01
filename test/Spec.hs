import ConfigSpec
import DocTest
import RequireSpec
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  _ <- DocTest.run
  defaultMain $
    testGroup
      "jetpack"
      [ testGroup "suites" [ConfigSpec.suite, RequireSpec.suite]
      , testGroup "properties" [RequireSpec.properties]
      ]
