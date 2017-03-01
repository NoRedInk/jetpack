import ConfigSpec
import RequireSpec
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $
  testGroup
    "jetpack"
    [ testGroup "suites" [ConfigSpec.suite, RequireSpec.suite]
    , testGroup "properties" [RequireSpec.properties]
    ]
