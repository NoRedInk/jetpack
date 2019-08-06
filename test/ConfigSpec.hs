module ConfigSpec where

import Config
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

suite :: TestTree
suite =
  testGroup
    "Config"
    [ testCase "#load success" $ do
        config <- Config.load "./test/fixtures"
        config @=?
          ( Config.Config
            { entryPoints = EntryPoints $ "app" </> "modules"
            , modulesDirs = []
            , sourceDir = SourceDir $ "app" </> "sources"
            , elmRoot = ElmRoot $ "app" </> "sources"
            , tempDir = TempDir $ "app" </> "tmp"
            , logDir = LogDir $ "app" </> "logs"
            , outputDir = OutputDir $ "app" </> "js"
            , elmPath = Nothing
            , coffeePath = Nothing
            , noParse = [ NoParse $
                            "." </>
                            "node_modules" </>
                            "clipboard" </>
                            "clipboard.js"
                        ]
            , watchFileExt = WatchFileExt <$> [".elm", ".coffee", ".js", ".json"]
            , watchIgnorePatterns = WatchIgnorePatterns <$> ["/[.]#[^/]*$", "/~[^/]*$"]
            , hotReloadingPort = Config.HotReloadingPort 31337
            }
          )
    ]
