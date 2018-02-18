{-|
Module      : Notify
Description : Notify's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Notify where

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Foldable
import Data.Text as T
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import System.FilePath
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process

foreign import ccall "watch_for_changes" watchForChanges ::
               CString -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "wrapper" mkCallback ::
               (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

watch :: T.Text -> [T.Text] -> IO () -> IO ()
watch path extensions callback = do
  mVar <- newMVar Nothing
  cb <- mkCallback $ forkCallback mVar callback extensions
  pathCStr <- newCString $ T.unpack path
  watchForChanges pathCStr cb

forkCallback :: MVar (Maybe ProcessID) -> IO () -> [T.Text] -> CString -> IO ()
forkCallback mVar cb extensions pathC = do
  eventForPath <- peekCString pathC
  when (isRelevant eventForPath extensions) $ do
    runningProcess <- takeMVar mVar
    traverse_ (signalProcess softwareTermination) runningProcess
    traverse_ (getProcessStatus True False) runningProcess -- here be dragons, potentially
    processId <- forkProcess cb
    putMVar mVar (Just processId)

isRelevant :: FilePath -> [T.Text] -> Bool
isRelevant path = elem (T.pack (takeExtension path))
