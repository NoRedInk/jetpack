module Version
  ( print
  , check
  ) where

import qualified Data.SemVer as SemVer
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Version (showVersion)
import qualified Parser.JetpackVersion as JetpackVersion
import Paths_jetpack (version)
import Prelude hiding (print)

print :: T.Text
print = T.pack $ showVersion version

check :: JetpackVersion.Version -> Maybe T.Text
check JetpackVersion.Version {version} =
  case SemVer.fromText print of
    Left _ ->
      Just
        "The version defined in your jetpack.json seems to be incorrect. Check your package.json to find the correct version."
    Right expected ->
      if expected == version
        then Nothing
        else Just
               ("Running jetpack@" <> print <> " the config expects " <>
                SemVer.toText version)
