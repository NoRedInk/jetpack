module Utils.Text where

import Protolude
import Data.Text as T

{-| Check if a text starts with a given prefix.

   >>> startsWith "--" "-- foo"
   True

   >>> startsWith "--" "-/ foo"
   False
-}
startsWith :: T.Text -> T.Text -> Bool
startsWith start text = start == textStart
  where
    len = T.length start
    (textStart, _) = T.splitAt len text
