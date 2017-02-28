import ConfigSpec
import RequireSpec
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $
  testGroup "jetpack"
    [ ConfigSpec.suite
    , RequireSpec.suite
    ]
