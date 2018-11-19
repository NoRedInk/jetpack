module Logger
  ( clearLog
  , appendLog
  , compileLog
  , consistencyLog
  , compileTime
  , allLogs
  ) where

import Config
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath ((<.>), (</>))

compileLog, compileTime, consistencyLog :: FilePath
compileLog = "compile" <.> "log"

compileTime = "compile" <.> "time"

consistencyLog = "elm-stuff__consistency" <.> "log"

allLogs :: [FilePath]
allLogs = [compileTime, compileLog, consistencyLog]

appendLog :: Config -> FilePath -> T.Text -> IO ()
appendLog Config {logDir} fileName msg =
  TIO.appendFile (logDir </> fileName) msg

clearLog :: Config -> FilePath -> IO ()
clearLog Config {logDir} fileName = TIO.writeFile (logDir </> fileName) ""
