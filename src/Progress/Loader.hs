module Progress.Loader
  ( start
  , stop
  ) where

import qualified Control.Concurrent as C
import qualified Data.Text as T
import qualified System.Console.Regions as CR

newtype Loader =
  Loader C.ThreadId

start :: CR.ConsoleRegion -> IO Loader
start region = do
  id <- C.forkIO (loader region (cycle loaderParts))
  return (Loader id)

stop :: Loader -> IO ()
stop (Loader id) = C.killThread id

loaderParts :: [T.Text]
loaderParts = [".  ", " . ", "  .", " . "]

loader :: CR.ConsoleRegion -> [T.Text] -> IO ()
loader region parts =
  CR.withConsoleRegion (CR.InLine region) (\r -> loader' r parts)

loader' :: CR.ConsoleRegion -> [T.Text] -> IO ()
loader' _ [] = return ()
loader' region (x:rest) = do
  CR.setConsoleRegion region x
  C.threadDelay 200000
  loader' region rest
