{-|
Module      : Notify
Description : Filesystem notifications. This is a wrapper around rust's notify crate.

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
  , runAtStartup :: Bool
  , onChange :: IO ()
  , onError :: T.Text -> IO ()
  }

watch :: Config -> IO ()
watch config = do
  mVar <- newMVar Nothing
  _ <- forkIO (start mVar config)
  listenToCommands mVar config

listenToCommands :: MVar (Maybe ProcessID) -> Config -> IO ()
listenToCommands mVar (config@Config {onChange}) = do
  value <- getChar
  case commandFromStr value of
    Just Rebuild -> do
      _ <- startProcess mVar onChange
      listenToCommands mVar config
    Just Quit -> do
      stopProcess mVar
      putStrLn "Thanks for compiling with jetpack today. Have a great day!"
    Just Help -> do
      _ <- putStrLn "Help"
      _ <- putStrLn "===="
      _ <- putStrLn ""
      _ <- putStrLn "r: rebuild"
      _ <- putStrLn "h: help"
      _ <- putStrLn "?: help"
      listenToCommands mVar config
    Just (Unknown str) -> do
      putStrLn ("Unknown" ++ (str : ""))
      listenToCommands mVar config
    Nothing -> listenToCommands mVar config

data Command
  = Rebuild
  | Quit
  | Help
  | Unknown Char

commandFromStr :: Char -> Maybe Command
commandFromStr 'r' = Just Rebuild
commandFromStr 'q' = Just Quit
commandFromStr 'h' = Just Help
commandFromStr '\n' = Nothing
commandFromStr char = Just (Unknown char)

start :: MVar (Maybe ProcessID) -> Config -> IO ()
start mVar Config { pathToWatch
                  , debounceInSecs
                  , relevantExtensions
                  , runAtStartup
                  , onChange
                  , onError
                  } = do
  when runAtStartup $ startProcess mVar onChange
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
