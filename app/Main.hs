module Main where

import Lib

main :: IO ()
main = someFunc
  -- 1. read config
  -- 2. find dependencies of entry points (magic requires (.sass/.elm)?)
  -- 3. build cache (not mvp)
  -- 4. compile
  -- 5. concat executable
