{-# LANGUAGE TemplateHaskell #-}

module HotReload
  ( inject
  , wrap
  )
where

import Protolude hiding ((<.>))
import qualified Config
import qualified Data.FileEmbed
import Data.Semigroup ((<>))
import qualified Data.Text as T
-- This code is directly ported from https://github.com/klazuka/elm-hot/blob/master/src/inject.js
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
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
    ]
  )

inject :: FilePath -> IO ()
inject path = do
  originalElmCodeJS <- TIO.readFile path
  let hmrCode = $(Data.FileEmbed.embedFile $ "resources" </> "hmr" <.> "js")
  let fixedNavKey = fixNavigationKey originalElmCodeJS
  case P.parse platformExportParser "" fixedNavKey of
    Left err -> print err
    Right (before, after) -> do
      let modifiedCode =
            T.unlines [before, "\n", E.decodeUtf8 hmrCode, "\n", after]
      Safe.IO.writeFile path modifiedCode

-- Attach a tag to Browser.Navigation.Key values.
-- We will add a property to the key immediately after it's created so that we can find it.
fixNavigationKey :: T.Text -> T.Text
fixNavigationKey code = do
  if T.isInfixOf "elm$browser$Browser$application" code
  then
    let navKeyDefinition =
          "var key = function() { key.a(onUrlChange(_Browser_getUrl())); };"
        navKeyTag = "key['elm-hot-nav-key'] = true"
        modifiedCode =
          T.replace
            navKeyDefinition
            (navKeyDefinition <> "\n" <> navKeyTag)
            code
     in modifiedCode
  else code

platformExportParser :: P.Parsec T.Text st (T.Text, T.Text)
platformExportParser = do
  let untilString = P.manyTill P.anyChar . P.try . P.string
      platformExport = "_Platform_export("
      this = "}(this));"
  before <- untilString platformExport
  platform <- untilString this
  return (T.pack $ before <> platformExport <> platform, T.pack this)
