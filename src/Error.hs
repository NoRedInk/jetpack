module Error
  ( Error (..)
  , description
  )
where

import Protolude hiding (show)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import System.FilePath ()
import Text.Show (show)

data Error
  = FileNotFound T.Text
  | JsonInvalid
      FilePath
      T.Text
  | NoModulesPresent [FilePath]
  | ModuleNotFound
      (Maybe FilePath)
      FilePath
  | BinNotFound T.Text
  | CompileError
      T.Text
      T.Text
  | HookFailed
      T.Text
      T.Text
  | ConfigInvalid
      FilePath
      T.Text
  | NoConfigFound FilePath
  deriving (Eq, Show)

description :: Error -> T.Text
description (FileNotFound file) = "Couldn't find file: " <> file
description (JsonInvalid file err) =
  T.unlines ["Invalid json file: " <> T.pack file, "", "    " <> err, ""]
description (ConfigInvalid file err) =
  T.unlines ["Invalid jetpack.json: " <> T.pack file, "", "    " <> err, ""]
description (NoModulesPresent paths) =
  T.unlines
    [ "It seems to me that you either provided a wrong `entry_points` or you don't have any modules."
    , ""
    , "I didn't find anything in " <> T.pack (show paths)
    , ""
    ]
description (ModuleNotFound (Just requiredIn) file) =
  T.unlines
    [ ""
    , ""
    , "I had troubles finding " <> T.pack file <> " required in " <>
      T.pack requiredIn <>
      "."
    ]
description (ModuleNotFound Nothing file) =
  T.unlines
    ["", "", "I had troubles finding the entry point " <> T.pack file <> "."]
description (BinNotFound bin) =
  T.unlines
    [ "I had troubles finding the " <> bin <> " command."
    , ""
    , "You might want to install it."
    ]
description (CompileError cmd msg) =
  T.unlines ["Command:", "", "    $ " <> cmd, "", msg]
description (HookFailed msg hookScript) =
  T.unlines ["Hook:", "", "    $ " <> hookScript, "", msg]
description (NoConfigFound path) =
  T.unlines ["I didn't find a config for jetpack at " <> T.pack path]
