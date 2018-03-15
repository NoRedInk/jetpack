module Version
  ( print
  , check
  ) where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_jetpack (version)
import Prelude hiding (print)

print :: T.Text
print = T.pack $ showVersion version

check :: T.Text -> Either T.Text ()
check version =
  if print == version
    then Right ()
    else Left ("Expected jetpack@" <> print <> " actual version is " <> version)
