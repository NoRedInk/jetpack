{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( clearLog, appendLog
  ) where


import Data.Text as T
import System.FilePath ((</>))
import Task

appendLog :: FilePath -> T.Text -> Task ()
appendLog logDir msg = toTask $ appendFile (logDir </> "jetpack.log") $ T.unpack msg

clearLog :: FilePath -> Task ()
clearLog logDir = toTask $ writeFile (logDir </> "jetpack.log") ""
