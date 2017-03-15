{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module ConcatModule where

import Data.Text as T


{-| Wraps a module in a function and injects require, module, exports.
    >>> :set -XOverloadedStrings
    >>> wrapModule "console.log(42);"
    "function(require, module, exports) {\nconsole.log(42);}\n"
-}
wrapModule :: T.Text -> T.Text
wrapModule "" = ""
wrapModule mod =
  T.concat
    [ "function(require, module, exports) {\n"
    , mod
    , "}\n"
    ]
