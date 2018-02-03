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

appendLog :: Env -> T.Text -> T.Text -> Task ()
appendLog Env {config} fileName msg = do
  let Config {log_directory} = config
  toTask $ appendFile (log_directory </> (T.unpack fileName)) $ T.unpack msg

clearLog :: Env -> T.Text -> Task ()
clearLog Env {config} fileName = do
  let Config {log_directory} = config
  toTask $ writeFile (log_directory </> (T.unpack fileName)) ""
