{-|
Module      : Notify
Description : Filesystem notifications. This is a wrapper around rust's notify crate.

-}
module Notify
  ( Config(..)
  , State
  , watch
  , force
  , end
  ) where

import Control.Concurrent
import Control.Monad (when)
import Data.Foldable (null, traverse_)
import Data.Int
import qualified Data.Text as T
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr ()
import Foreign.Ptr
import System.FilePath
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process ()

foreign import ccall "watch_for_changes" watchForChanges ::
               CString ->
                 CInt ->
                   FunPtr (CString -> IO ()) -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "wrapper" mkCallback ::
               (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

{-| Internal state of the watcher.
We keep track of running processes and the config.
You might need this if you want to use `end` or `force`.
-}
data State = State
  { config :: Config
  , mVar :: MVar (Maybe ProcessID)
  }

{-| Configuration for a watcher.
-}
data Config = Config
  { pathToWatch :: FilePath -- Watch files recursivelly under this path.
  , relevantExtensions :: [T.Text] -- Which extensions do we care about? Empty list will accept all.
  , debounceInSecs :: Int -- Debounce next run by x seconds.
  , onChange :: IO () -- callback on filesystem changes.
  , onError :: T.Text -> IO () -- callback for errors.
  }

watch :: Config -> IO State
watch config = do
  mVar <- newMVar Nothing
  let state = State {config = config, mVar = mVar}
  _ <- forkIO (start mVar config)
  pure state

force :: State -> IO ()
force State {mVar, config} = startProcess mVar (onChange config)

end :: State -> IO ()
end State {mVar} = stopProcess mVar

start :: MVar (Maybe ProcessID) -> Config -> IO ()
start mVar Config { pathToWatch
                  , debounceInSecs
                  , relevantExtensions
                  , onChange
                  , onError
                  } = do
  onChangeCb <- mkCallback $ callbackInProcess mVar onChange relevantExtensions
  onErrorCb <- mkCallback $ onErrorCallback onError
  pathCStr <- newCString pathToWatch
  watchForChanges pathCStr (mkCInt debounceInSecs) onChangeCb onErrorCb

mkCInt :: Int -> CInt
mkCInt = fromIntegral

onErrorCallback :: (T.Text -> IO ()) -> CString -> IO ()
onErrorCallback cb msgC = do
  msg <- peekCString msgC
  cb (T.pack msg)

callbackInProcess ::
     MVar (Maybe ProcessID) -> IO () -> [T.Text] -> CString -> IO ()
callbackInProcess mVar cb relevantExtensions pathC = do
  eventForPath <- peekCString pathC
  when (isRelevant eventForPath relevantExtensions || null relevantExtensions) $
    startProcess mVar cb

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

isRelevant :: FilePath -> [T.Text] -> Bool
isRelevant path = elem (T.pack (takeExtension path))
