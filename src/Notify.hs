module Notify
  ( Config(..)
  , State
  , watch
  , buildNow
  , end
  ) where

import Control.Concurrent
import Data.Foldable (traverse_)
import qualified Data.Text as T
import System.FSNotify
import System.FilePath
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process ()
import Text.Regex (matchRegex, Regex)


{-| Internal state of the watcher.
We keep track of running processes and the config.
You might need this if you want to use `end` or `force`.
-}
data State = State
  { onChange :: IO ()
  , mVar :: MVar (Maybe ProcessID)
  }

{-| Configuration for a watcher.
-}
data Config = Config
  { pathToWatch :: FilePath -- Watch files recursivelly under this path.
  , relevantExtensions :: [T.Text] -- Which extensions do we care about? Empty list will accept all.
  , ignorePatterns :: [Regex] -- Which filename patterns do we want to ignore? Empty list will accept all.
  }

watch :: Config -> IO () -> IO State
watch config onChange = do
  mVar <- newMVar Nothing
  let state = State {onChange = onChange, mVar = mVar}
  _ <- forkIO (start mVar config onChange)
  pure state

start :: MVar (Maybe ProcessID) -> Config -> IO () -> IO ()
start mVar Config {pathToWatch, relevantExtensions, ignorePatterns} onChange = do
  manager <-
    startManagerConf
      (WatchConfig
       { confDebounce = Debounce 0.2
       , confUsePolling = False
       , confPollInterval = 10 ^ (6 :: Int)
       })
  _ <-
    watchTree
      manager
      pathToWatch
      (eventIsRelevant relevantExtensions ignorePatterns)
      (actOnEvent mVar onChange)
  pure ()

eventIsRelevant :: [T.Text] -> [Regex] -> Event -> Bool
eventIsRelevant relevantExtensions ignorePatterns event =
  getExtensionFromEvent event `elem` relevantExtensions &&
  getFilepathFromEvent event `matchesNone` ignorePatterns

getExtensionFromEvent :: Event -> T.Text
getExtensionFromEvent = T.pack . takeExtension . getFilepathFromEvent

actOnEvent :: MVar (Maybe ProcessID) -> IO () -> Event -> IO ()
actOnEvent mVar onChange _event = startProcess mVar onChange

startProcess :: MVar (Maybe ProcessID) -> IO () -> IO ()
startProcess mVar cb = do
  stopProcess mVar
  processId <- forkProcess cb
  putMVar mVar (Just processId)

stopProcess :: MVar (Maybe ProcessID) -> IO ()
stopProcess mVar = do
  runningProcess <- takeMVar mVar
  traverse_ (signalProcess softwareTermination) runningProcess
  traverse_ (getProcessStatus True False) runningProcess -- here be dragons, potentially

buildNow :: State -> IO ()
buildNow State {mVar, onChange} = startProcess mVar onChange

end :: State -> IO ()
end State {mVar} = stopProcess mVar

getFilepathFromEvent :: Event -> FilePath
getFilepathFromEvent (Added filepath _) = filepath
getFilepathFromEvent (Modified filepath _) = filepath
getFilepathFromEvent (Removed filepath _) = filepath

matchesNone :: FilePath -> [Regex] -> Bool
matchesNone filepath =
  not . any (matches filepath)
  where
    matches filepath regex =
      case matchRegex regex filepath of
        Just _ -> True
        Nothing -> False
