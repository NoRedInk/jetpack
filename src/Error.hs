module Error
  ( Error(..)
  , description
  ) where

import qualified Data.List as L
import System.FilePath ()

data Error
  = FileNotFound String
  | JsonInvalid String
                String
  | NoModulesPresent String
  | ModuleNotFound (Maybe FilePath)
                   String
  | BinNotFound String
  | CompileError String
                 String
  | HookFailed String
               String
  | ConfigInvalid String
                  String
  deriving (Eq, Show)

description :: Error -> String
description (FileNotFound file) = "Couldn't find file: " ++ file
description (JsonInvalid file err) =
  L.unlines ["Invalid json file: " ++ file, "", "    " ++ err, ""]
description (ConfigInvalid file err) =
  if "key \"version\" not present" `L.isInfixOf` err
    then L.unlines
           [ "Invalid jetpack.json: " ++ file
           , ""
           , "    " ++ err
           , ""
           , "    `version` was added in jetpack@2.0.15. It's recommended to upgrade to the latest version."
           , "    `version` should point to the jetpack version you expect to build your app."
           , "    This will inform users when they need to upgrade/downgrade jetpack in order to create a correct build."
           ]
    else L.unlines ["Invalid jetpack.json: " ++ file, "", "    " ++ err, ""]
description (NoModulesPresent path) =
  L.unlines
    [ "It seems to me that you either provided a wrong `entry_points` or you don't have any modules."
    , ""
    , "I didn't find anything in " ++ path
    , ""
    ]
description (ModuleNotFound (Just requiredIn) file) =
  L.unlines
    [ ""
    , ""
    , "I had troubles finding " ++ file ++ " required in " ++ requiredIn ++ "."
    ]
description (ModuleNotFound Nothing file) =
  L.unlines ["", "", "I had troubles finding the entry point " ++ file ++ "."]
description (BinNotFound bin) =
  L.unlines
    [ "I had troubles finding the " ++ bin ++ " command."
    , ""
    , "You might want to install it."
    ]
description (CompileError cmd msg) =
  L.unlines ["Command:", "", "    $ " ++ cmd, "", msg]
description (HookFailed msg hookScript) =
  L.unlines ["Hook:", "", "    $ " ++ show hookScript, "", msg]
