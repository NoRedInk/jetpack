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

compileLog, compileTime, preHookLog, postHookLog :: T.Text
compileLog = "compile.log"

compileTime = "compile.time"

preHookLog = "pre-hook.log"

postHookLog = "post-hook.log"

allLogs :: [T.Text]
allLogs = [compileTime, compileLog, preHookLog, postHookLog]

appendLog :: Config -> T.Text -> T.Text -> IO ()
appendLog config fileName msg = do
  let Config {log_directory} = config
  appendFile (log_directory </> T.unpack fileName) $ T.unpack msg

clearLog :: Config -> T.Text -> IO ()
clearLog Config {log_directory} fileName = do
  writeFile (log_directory </> T.unpack fileName) ""
