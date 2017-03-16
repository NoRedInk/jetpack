{-# LANGUAGE OverloadedStrings #-}

module Error
  ( Error(..)
  , description
  ) where

import qualified Data.List as L
import System.FilePath ((</>))

data Error
  = FileNotFound String
  | JsonInvalid String
  | NoModulesPresent String
  | ModuleNotFound FilePath FilePath String
  | BinNotFound String
  deriving (Eq, Show)

-- TODO this should be Show
description :: Error -> String
description (FileNotFound file) = "Couldn't find file: " ++ file
description (JsonInvalid file) = "Invalid json file: " ++ file
description (NoModulesPresent path) =
  L.unlines
    [ "It seems to me that you either provided a wrong `module_directory` or you don't have any modules."
    , ""
    , "I didn't find anything in " ++ path
    , ""
    ]
description (ModuleNotFound moduleDirectory sourceDirectory file) =
  L.unlines
    [ "I had troubles finding " ++ file ++ "."
    , ""
    , "I looked in:"
    , show $ moduleDirectory
    , show $ sourceDirectory
    , show $ sourceDirectory </> ".." </> "node_modules"
    , ""
    , "In addition I tried it with a `.js` extension and with `/index.js`"
    , ""
    ]
description (BinNotFound bin) =
  L.unlines
    [ "I had troubles finding the " ++ bin ++ " command."
    , ""
    , "You might want to install it."
    ]
