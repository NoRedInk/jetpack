module Watcher
  ( watch
  , startWatcher
  , listenToCommands
  )
where

import qualified Builder
import CliArguments (Args (..))
import Config (Config (Config))
import qualified Config
import Control.Monad (void)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Notify
import System.FilePath ()
import Text.Regex (mkRegex)

watch :: Config -> Args -> Builder.HotReload -> IO ()
watch config args hotReloading = do
  putStrLn "Watching. Enter '?' to see the help."
  state <- startWatcher config (void $ Builder.build config args hotReloading)
  Notify.buildNow state
  listenToCommands state

startWatcher :: Config -> IO () -> IO Notify.State
startWatcher
  Config
    { Config.sourceDir
    , Config.watchFileExt
    , Config.watchIgnorePatterns
    } =
    Notify.watch
      Notify.Config
        { pathToWatch = Config.unSourceDir sourceDir
        , relevantExtensions = Config.unWatchFileExt <$> watchFileExt
        , ignorePatterns = mkRegex . T.unpack <$> Config.unWatchIgnorePatterns <$>
          watchIgnorePatterns
        }

listenToCommands :: Notify.State -> IO ()
listenToCommands state = do
  value <- TIO.getLine
  case commandFromStr value of
    Just Rebuild -> do
      _ <- TIO.putStrLn "Forcing a rebuild..."
      _ <- Notify.buildNow state
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
