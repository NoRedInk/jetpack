module Watcher
  ( watch
  ) where

import qualified Builder
import CliArguments (Args(..))
import qualified Compile
import Config (Config(Config))
import qualified Config
import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets as WS
import qualified Notify
import System.FilePath ()
import Text.Regex (mkRegex)

watch :: Config -> Args -> Builder.HotReload -> IO ()
watch config@Config { Config.sourceDir
                    , Config.watchFileExt
                    , Config.watchIgnorePatterns
                    } args hotReloading = do
  putStrLn "Watching. Enter '?' to see the help."
  case hotReloading of
    Builder.HotReload -> hotReloadingServer config args hotReloading
    Builder.DontHotReload -> do
      state <-
        Notify.watch
          Notify.Config
          { pathToWatch = Config.unSourceDir sourceDir
          , relevantExtensions = Config.unWatchFileExt <$> watchFileExt
          , ignorePatterns =
              mkRegex . T.unpack <$> Config.unWatchIgnorePatterns <$>
              watchIgnorePatterns
          }
          (void $ Builder.build config args hotReloading)
      Notify.buildNow state
      listenToCommands state

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

hotReloadingServer :: Config -> Args -> Builder.HotReload -> IO ()
hotReloadingServer config args hotReloading = do
  putStrLn "Wait until the first build succeeded!"
  putStrLn "Refresh your browser as soon as the build was successful."
  _ <- Builder.build config args hotReloading
  WS.runServer
    "127.0.0.1"
    (Config.unHotReloadingPort $ Config.hotReloadingPort config) $
    application config args hotReloading

application :: Config -> Args -> Builder.HotReload -> WS.ServerApp
application config@Config { Config.sourceDir
                          , Config.watchFileExt
                          , Config.watchIgnorePatterns
                          } args hotReloading pending = do
  putStrLn "Hot-reloading server is ready."
  conn <- WS.acceptRequest pending
  state <-
    Notify.watch
      Notify.Config
      { pathToWatch = Config.unSourceDir sourceDir
      , relevantExtensions = Config.unWatchFileExt <$> watchFileExt
      , ignorePatterns =
          mkRegex . T.unpack <$> Config.unWatchIgnorePatterns <$>
          watchIgnorePatterns
      }
      (do maybeResult <- Builder.build config args hotReloading
          case maybeResult of
            Nothing -> pure ()
            Just result -> traverse_ (hotReload conn) $ Compile.elmFiles result)
  listenToCommands state

hotReload :: WS.Connection -> FilePath -> IO ()
hotReload conn filePath = do
  content <- readFile filePath
  WS.sendTextData conn $
    T.unlines
      [ "jetpack___hot__reloading();"
      , "function jetpack___hot__reloading() {"
      , T.pack content
      , "}"
      ]
