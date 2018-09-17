module Logger
  ( clearLog
  , appendLog
  , compileLog
  , compileTime
  , allLogs
  ) where

import Config
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((<.>), (</>))

compileLog, compileTime :: FilePath
compileLog = "compile" <.> "log"

compileTime = "compile" <.> "time"

allLogs :: [FilePath]
allLogs = [compileTime, compileLog]

appendLog :: Config -> FilePath -> T.Text -> IO ()
appendLog Config {logDir} fileName msg =
  TIO.appendFile (logDir </> fileName) msg

clearLog :: Config -> FilePath -> IO ()
clearLog Config {logDir} fileName = TIO.writeFile (logDir </> fileName) ""
