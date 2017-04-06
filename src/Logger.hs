{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( clearLog, appendLog
  ) where

import Control.Monad.Trans.Class (lift)
import Data.Text as T
import Task

appendLog :: T.Text -> Task ()
appendLog msg = lift $ appendFile ".jetpack.log" $ T.unpack msg

clearLog :: Task ()
clearLog  = lift $ writeFile ".jetpack.log" ""
