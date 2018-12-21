module HotReload
  ( inject
  , wrap
  ) where

import qualified Config
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Paths_jetpack
import qualified Safe.IO
import System.FilePath ((<.>), (</>))
import qualified Text.Parsec as P

wrap :: Config.HotReloadingPort -> (a, T.Text) -> (a, T.Text)
wrap hotReloadingPort (a, content) =
  ( a
  , T.unlines
      [ "// Expose the Webpack HMR API"
      , "var myDisposeCallback = function() {};"
      , "// simulate the HMR api exposed by webpack"
      , "var moduleHot = {"
      , "    hot: {"
      , "        accept: function () {"
      , "        },"
      , "        dispose: function (callback) {"
      , "            myDisposeCallback = callback"
      , "        },"
      , "        data: null,"
      , "        apply: function () {"
      , "            var newData = {};"
      , "            myDisposeCallback(newData);"
      , "            moduleHot.hot.data = newData"
      , "        }"
      , "    }"
      , "};"
      , content
      , "// Listen for data from the websocket. When we get a message, eval it."
      , "var socketHotReloading = new WebSocket('ws://localhost:" <>
        T.pack (show $ Config.unHotReloadingPort hotReloadingPort) <>
        "');"
      , "socketHotReloading.onmessage = function(event) {"
      , "  console.warn('Jetpack reloading...');"
      , "  moduleHot.hot.apply();"
      , "  delete window.Elm;"
      , "  try {"
      , "    eval(event.data);"
      , "  } catch (e) {"
      , "    console.warn('Jetpack reloading failed!');"
      , "    console.error(e);"
      , "  };"
      , "  console.warn('Jetpack reloaded!');"
      , "};"
      ])

inject :: FilePath -> IO ()
inject path = do
  originalElmCodeJS <- readFile path
  hmrCodePath <- getDataFileName $ "resources" </> "hmr" <.> "js"
  hmrCode <- readFile hmrCodePath
    -- TODO SPA
    --
    -- if (originalElmCodeJS.indexOf("elm$browser$Browser$application") !== -1) {
    --     // attach a tag to Browser.Navigation.Key values. It's not really fair to call this a hack
    --     // as this entire project is a hack, but this is evil evil evil. We need to be able to find
    --     // the Browser.Navigation.Key in a user's model so that we do not swap out the new one for
    --     // the old. But as currently implemented (2018-08-19), there's no good way to detect it.
    --     // So we will add a property to the key immediately after it's created so that we can find it.
    --     const navKeyDefinition = "var key = function() { key.a(onUrlChange(_Browser_getUrl())); };";
    --     const navKeyTag = "key['elm-hot-nav-key'] = true";
    --     modifiedCode = originalElmCodeJS.replace(navKeyDefinition, navKeyDefinition + "\n" + navKeyTag);
    --     if (modifiedCode === originalElmCodeJS) {
    --         throw new Error("[elm-hot] Browser.Navigation.Key def not found. Version mismatch?");
    --     }
    -- }
  case P.parse platformExportParser "" $ T.pack originalElmCodeJS of
    Left err -> print err
    Right (before, after) -> do
      let modifiedCode = T.unlines [before, "\n", T.pack hmrCode, "\n", after]
      Safe.IO.writeFile path modifiedCode

platformExportParser :: P.Parsec T.Text st (T.Text, T.Text)
platformExportParser = do
  let untilString = P.manyTill P.anyChar . P.try . P.string
      platformExport = "_Platform_export("
      this = "}(this));"
  before <- untilString platformExport
  platform <- untilString this
  return (T.pack $ before <> platformExport <> platform, T.pack this)
