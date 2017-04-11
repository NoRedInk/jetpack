{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( clearLog, appendLog
  ) where


import Config
import Data.Text as T
import System.FilePath ((</>))
import Task

appendLog :: T.Text -> Task ()
appendLog msg = do
  Config {log_directory} <- Task.getConfig
  toTask $ appendFile (log_directory </> "jetpack.log") $ T.unpack msg

clearLog :: Task ()
clearLog = do
  Config {log_directory} <- Task.getConfig
  toTask $ writeFile (log_directory </> "jetpack.log") ""
