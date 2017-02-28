module Main where

import Lib

main :: IO ()
main = someFunc
  -- 1. read config
  -- 2. find dependencies of entry points (magic requires (.sass/.elm)?) (how many levels? we should probably find elm deps as wel)
  -- 3. create binary with tree
  -- 3. build cache (not mvp)
  --    a. create binary with file modified date and hash of content
  -- 4. compile
  --    a. check cache if we need to build this. first date and the hash
  -- 5. recurisvely replace requires
  -- 6. compress (if prod)
