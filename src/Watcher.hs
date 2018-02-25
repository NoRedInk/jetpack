module Watcher where

import qualified Builder
import CliArguments (Args(..))
import Config
import Control.Concurrent
import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Error
import GHC.IO.Handle
import qualified Message
import qualified Notify
import Notify (Config(..))
import qualified System.Directory as Dir
import System.Environment
import System.Exit
import System.FilePath ()
import qualified System.Posix.IO as PIO
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Process

watch :: Config.Config -> Args -> IO ()
watch config args = do
  putStrLn "Watching. Enter '?' to see the help."
  state <-
    Notify.watch
      Notify.Config
      { pathToWatch = Config.source_directory config
      , relevantExtensions =
          [".elm", ".coffee", ".js", ".sass", ".scss", ".json"]
      , debounceInSecs = 0
      , onChange = Builder.build config args
      , onError = \msg -> Message.printError [msg]
      }
  Notify.force state
  listenToCommands state

listenToCommands :: Notify.State -> IO ()
listenToCommands state = do
  value <- getChar
  case commandFromStr value of
    Just Rebuild -> do
      _ <- putStrLn "focing a rebuild..."
      _ <- Notify.force state
      listenToCommands state
    Just Quit -> do
      Notify.end state
      putStrLn "Thanks for compiling with jetpack today. Have a great day!"
    Just Help -> do
      _ <- putStrLn "Help"
      _ <- putStrLn "===="
      _ <- putStrLn ""
      _ <- putStrLn "r: rebuild"
      _ <- putStrLn "q: quit"
      _ <- putStrLn "?: help"
      listenToCommands state
    Just (Unknown str) -> do
      putStrLn ("Unknown" ++ (str : ""))
      listenToCommands state
    Nothing -> listenToCommands state

data Command
  = Rebuild
  | Quit
  | Help
  | Unknown Char

commandFromStr :: Char -> Maybe Command
commandFromStr 'r' = Just Rebuild
commandFromStr 'q' = Just Quit
commandFromStr '?' = Just Help
commandFromStr '\n' = Nothing
commandFromStr char = Just (Unknown char)
