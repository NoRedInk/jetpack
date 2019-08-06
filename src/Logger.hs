module Logger
  ( clearLog
  , appendLog
  , compileLog
  , consistencyLog
  , compileTime
  , allLogs
  )
where

import qualified Config
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Safe.IO
import System.FilePath ((<.>), (</>))

compileLog, compileTime, consistencyLog :: FilePath
compileLog = "compile" <.> "log"

compileTime = "compile" <.> "time"

consistencyLog = "elm-stuff__consistency" <.> "log"

allLogs :: [FilePath]
allLogs = [compileTime, compileLog, consistencyLog]

appendLog :: Config.LogDir -> FilePath -> T.Text -> IO ()
appendLog logDir fileName msg =
  TIO.appendFile (Config.unLogDir logDir </> fileName) msg

clearLog :: Config.LogDir -> FilePath -> IO ()
clearLog logDir fileName =
  Safe.IO.writeFile (Config.unLogDir logDir </> fileName) ""
