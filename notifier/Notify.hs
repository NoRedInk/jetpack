{-|
Module      : Notify
Description : Notify's main module

This is a haddock comment describing your library
For more information on how to write Haddock comments check the user guide:
<https://www.haskell.org/haddock/doc/html/index.html>
-}
module Notify
  ( Config(..)
  , watch
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

data Config = Config
  { pathToWatch :: FilePath
  , relevantExtensions :: [T.Text]
  , debounceInSecs :: Int
  , onChange :: IO ()
  , onError :: T.Text -> IO ()
  }

watch :: Config -> IO ()
watch Config { pathToWatch
             , debounceInSecs
             , relevantExtensions
             , onChange
             , onError
             } = do
  mVar <- newMVar Nothing
  onChangeCb <- mkCallback $ forkCallback mVar onChange relevantExtensions
  onErrorCb <- mkCallback $ onErrorCallback onError
  pathCStr <- newCString pathToWatch
  watchForChanges pathCStr (mkCInt debounceInSecs) onChangeCb onErrorCb

mkCInt :: Int -> CInt
mkCInt = fromIntegral

onErrorCallback :: (T.Text -> IO ()) -> CString -> IO ()
onErrorCallback cb msgC = do
  msg <- peekCString msgC
  cb (T.pack msg)

forkCallback :: MVar (Maybe ProcessID) -> IO () -> [T.Text] -> CString -> IO ()
forkCallback mVar cb relevantExtensions pathC = do
  eventForPath <- peekCString pathC
  when (isRelevant eventForPath relevantExtensions || null relevantExtensions) $ do
    runningProcess <- takeMVar mVar
    traverse_ (signalProcess softwareTermination) runningProcess
    traverse_ (getProcessStatus True False) runningProcess -- here be dragons, potentially
    processId <- forkProcess cb
    putMVar mVar (Just processId)

isRelevant :: FilePath -> [T.Text] -> Bool
isRelevant path = elem (T.pack (takeExtension path))
