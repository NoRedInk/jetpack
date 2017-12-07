{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( clearLog
  , appendLog
  , compileLog
  , preHookLog
  , postHookLog
  , allLogs
  ) where

import Config
import Data.Text as T
import System.FilePath ((</>))
import Task

compileLog, preHookLog, postHookLog :: T.Text
compileLog = "compile.log"

preHookLog = "pre-hook.log"

postHookLog = "post-hook.log"

allLogs :: [T.Text]
allLogs = [compileLog, preHookLog, postHookLog]

appendLog :: T.Text -> T.Text -> Task ()
appendLog fileName msg = do
  Config {log_directory} <- Task.getConfig
  toTask $ appendFile (log_directory </> (T.unpack fileName)) $ T.unpack msg

clearLog :: T.Text -> Task ()
clearLog fileName = do
  Config {log_directory} <- Task.getConfig
  toTask $ writeFile (log_directory </> (T.unpack fileName)) ""
