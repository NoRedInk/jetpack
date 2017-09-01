{-# LANGUAGE OverloadedStrings #-}
module Version where


import qualified Data.Text as T

print :: T.Text
print =
  T.concat ["🚀  Version: ", current]

{-| This version string is updated by ./release.sh.

===========================
== DON'T EDIT BY HAND!!! ==
===========================

-}
current :: T.Text
current = "0.4.0"
