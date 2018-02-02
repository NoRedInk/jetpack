module Logger
  ( clearLog
  , appendLog
  , compileLog
  , compileTime
  , preHookLog
  , postHookLog
  , allLogs
  ) where

import Config
import Data.Text as T
import System.FilePath ((</>))
import Task

compileLog, compileTime, preHookLog, postHookLog :: T.Text
compileLog = "compile.log"

compileTime = "compile.time"

preHookLog = "pre-hook.log"

postHookLog = "post-hook.log"

allLogs :: [T.Text]
allLogs = [compileTime, compileLog, preHookLog, postHookLog]

appendLog :: T.Text -> T.Text -> Task ()
appendLog fileName msg = do
  Config {log_directory} <- Task.getConfig
  toTask $ appendFile (log_directory </> (T.unpack fileName)) $ T.unpack msg

clearLog :: T.Text -> Task ()
clearLog fileName = do
  Config {log_directory} <- Task.getConfig
  toTask $ writeFile (log_directory </> (T.unpack fileName)) ""
