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
appendLog Config {logDir} fileName msg =
  TIO.appendFile (logDir </> fileName) msg

clearLog :: Config -> FilePath -> IO ()
clearLog Config {logDir} fileName = TIO.writeFile (logDir </> fileName) ""
