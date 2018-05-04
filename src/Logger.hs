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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((<.>), (</>))

compileLog, compileTime, preHookLog, postHookLog :: FilePath
compileLog = "compile" <.> "log"

compileTime = "compile" <.> "time"

preHookLog = "pre-hook" <.> "log"

postHookLog = "post-hook" <.> "log"

allLogs :: [FilePath]
allLogs = [compileTime, compileLog, preHookLog, postHookLog]

appendLog :: Config -> FilePath -> T.Text -> IO ()
appendLog Config {log_directory} fileName msg =
  TIO.appendFile (log_directory </> fileName) msg

clearLog :: Config -> FilePath -> IO ()
clearLog Config {log_directory} fileName =
  TIO.writeFile (log_directory </> fileName) ""
