module DocTest where

import System.FilePath.Glob (glob)
import Test.DocTest

run :: IO ()
run = do
  glob "src/**/*.hs" >>= doctest
