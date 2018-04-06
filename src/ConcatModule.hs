{-| Concat all modules required in an entrypoint into one file.
-}
module ConcatModule
  ( wrap
  ) where

import Config

import qualified Data.List.Utils as LU
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Tree as Tree
import Dependencies (Dependency(..), DependencyTree)
import ProgressBar (ProgressBar, tick)
import System.Directory (createDirectoryIfMissing)
import System.FilePath as FP
import Text.Regex (mkRegex, subRegex)
import qualified Utils.Files as F
import qualified Utils.Tree as UT

wrap :: ProgressBar -> Config -> DependencyTree -> IO FilePath
wrap pg config dep = do
  depsWithContent <- traverse (withContent config) $ uniqNodes dep
  let wrapped = fmap wrapDependency depsWithContent
  out <- writeJsModule config wrapped $ filePath $ Tree.rootLabel dep
  _ <- tick pg
  return out

uniqNodes :: DependencyTree -> [(Dependency, [Dependency])]
uniqNodes = LU.uniq . UT.nodesWithChildren

withContent ::
     Config -> (Dependency, [Dependency]) -> IO (FilePath, [Dependency], T.Text)
withContent Config {temp_directory} (Dependency {filePath}, ds) = do
  let name = F.pathToFileName filePath "js"
  content <- readFile $ temp_directory </> name
  return (filePath, ds, T.pack content)

wrapDependency :: (FilePath, [Dependency], T.Text) -> T.Text
wrapDependency (filePath, ds, content) =
  wrapModule
    (F.pathToFunctionName filePath "js")
    (foldr replaceRequire (content) ds)

replaceRequire :: Dependency -> T.Text -> T.Text
replaceRequire Dependency {requiredAs, filePath} body =
  T.pack $ subRegex requireRegex (T.unpack body) jetpackRequire
  where
    fnName = T.unpack $ F.pathToFunctionName filePath "js"
    requireRegex =
      mkRegex $ "require\\([ \t]*['\"]" <> requiredAs <> "['\"][ \t]*\\)"
    jetpackRequire = "jetpackRequire(" <> fnName <> ", \"" <> fnName <> "\")"

writeJsModule :: Config -> [T.Text] -> FilePath -> IO FilePath
writeJsModule Config {output_js_directory, entry_points} fns rootFilePath = do
  let out = output_js_directory </> FP.makeRelative entry_points rootFilePath
  let rootName = F.pathToFunctionName rootFilePath "js"
  createDirectoryIfMissing True $ FP.takeDirectory out
  writeFile out $ T.unpack $ addBoilerplate rootName fns
  return out

addBoilerplate :: T.Text -> [T.Text] -> T.Text
addBoilerplate root fns =
  T.unlines
    [ "(function() {"
    , "var jetpackCache = {};"
    , "function jetpackRequire(fn, fnName) {"
    , "  var e = {};"
    , "  var m = { exports : e };"
    , "  if (typeof fn !== \"function\") {"
    , "    console.error(\"Required function isn't a jetpack module.\", fn)"
    , "    return;"
    , "  }"
    , "  if (jetpackCache[fnName]) {"
    , "    return jetpackCache[fnName];"
    , "  }"
    , "  jetpackCache[fnName] = m.exports;"
    , "  fn(m, e);  "
    , "  jetpackCache[fnName] = m.exports;"
    , "  return m.exports;"
    , "}"
    , T.concat fns
    , T.concat ["jetpackRequire(", root, ", \"", root, "\");"] -- calling the entry point
    , "})();"
    ]

{-| Wraps a module in a function and injects require, module, exports.
    >>> wrapModule "foo" "console.log(42);"
    "/* START: foo */\nfunction foo(module, exports) {\nconsole.log(42);\n} /* END: foo */\n"
-}
wrapModule :: T.Text -> T.Text -> T.Text
wrapModule fnName body =
  T.concat
    [ "/* START: "
    , fnName
    , " */"
    , "\n"
    , T.concat ["function ", fnName, "(module, exports) {\n"]
    , if body == ""
        then "  console.warn(\"" <> fnName <> ": is an empty module!\");"
        else body
    , "\n} /* END: "
    , fnName
    , " */"
    , "\n"
    ]
