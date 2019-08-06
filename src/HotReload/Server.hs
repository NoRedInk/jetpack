module HotReload.Server
  ( start
  )
where

import Protolude
import qualified Builder
import CliArguments (Args (..))
import qualified Compile
import qualified Config
import Config (Config)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import qualified Watcher
import Data.String (String)

start :: Config -> Args -> Builder.HotReload -> IO ()
start config args hotReloading = do
  putStrLn ("Wait until the first build succeeded!" :: String)
  putStrLn ("Refresh your browser as soon as the build was successful." ::  String)
  _ <- Builder.build config args hotReloading
  WS.runServer
    "127.0.0.1"
    (Config.unHotReloadingPort $ Config.hotReloadingPort config) $
    application config args hotReloading

application :: Config -> Args -> Builder.HotReload -> WS.ServerApp
application config args hotReloading pending = do
  putStrLn ("Hot-reloading server is ready." :: String)
  conn <- WS.acceptRequest pending
  state <-
    Watcher.startWatcher
      config
      ( do
        maybeResult <- Builder.build config args hotReloading
        case maybeResult of
          Nothing -> pure ()
          Just result -> traverse_ (reload conn) $ Compile.elmFiles result
      )
  Watcher.listenToCommands state

reload :: WS.Connection -> FilePath -> IO ()
reload conn filePath = do
  content <- readFile filePath
  WS.sendTextData conn $
    T.unlines
      [ "jetpack___hot__reloading();"
      , "function jetpack___hot__reloading() {"
      , content
      , "}"
      ]
