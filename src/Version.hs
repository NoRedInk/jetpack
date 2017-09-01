{-# LANGUAGE OverloadedStrings #-}
module Version where


import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_jetpack (version)

print :: T.Text
print = T.pack $ showVersion version
