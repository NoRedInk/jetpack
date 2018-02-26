module Watcher where

import qualified Builder
import CliArguments (Args(..))
import Config
import Control.Concurrent
import Control.Monad.Except (throwError)
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
      }
      (Builder.build config args)
      (\msg -> Message.printError [msg])
  Notify.force state
  listenToCommands state

listenToCommands :: Notify.State -> IO ()
listenToCommands state = do
  value <- TIO.getLine
  case commandFromStr value of
    Just Rebuild -> do
      _ <- TIO.putStrLn "focing a rebuild..."
      _ <- Notify.force state
      listenToCommands state
    Just Quit -> do
      Notify.end state
      putStrLn "Thanks for compiling with jetpack today. Have a great day!"
    Just Help -> do
      _ <- TIO.putStrLn "Help"
      _ <- TIO.putStrLn "===="
      _ <- TIO.putStrLn ""
      _ <- TIO.putStrLn "r: rebuild"
      _ <- TIO.putStrLn "q: quit"
      _ <- TIO.putStrLn "?: help"
      listenToCommands state
    Just (Unknown str) -> do
      TIO.putStrLn ("Unknown command \"" <> str <> "\"")
      listenToCommands state
    Nothing -> listenToCommands state

data Command
  = Rebuild
  | Quit
  | Help
  | Unknown T.Text

commandFromStr :: T.Text -> Maybe Command
commandFromStr "r" = Just Rebuild
commandFromStr "q" = Just Quit
commandFromStr "?" = Just Help
commandFromStr "" = Nothing
commandFromStr char = Just (Unknown char)
