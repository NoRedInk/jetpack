{-| Concat all modules required in an entrypoint into one file.
-}
module ConcatModule
  ( wrap
  , wrapModule
  , replaceRequire
  ) where

import Config
import Data.Char (isSpace)
import Data.Foldable (all)
import qualified Data.List.Utils as LU
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Tree as Tree
import Dependencies (Dependency(..), DependencyTree)
import qualified Parser.Ast as Ast
import ProgressBar (ProgressBar, tick)
import System.Directory (createDirectoryIfMissing)
import System.FilePath as FP
import Text.Regex (mkRegex, subRegex)
import qualified Utils.Files as F
import qualified Utils.Tree as UT

wrap :: ProgressBar -> Config -> DependencyTree -> IO FilePath
wrap pg config dep = do
  module' <- traverse (withContent config) $ uniqNodes dep
  let wrapped = fmap wrapDependency module'
  out <-
    writeJsModule config wrapped $ Dependencies.filePath $ Tree.rootLabel dep
  _ <- tick pg
  return out

uniqNodes :: DependencyTree -> [(Dependency, [Dependency])]
uniqNodes = LU.uniq . UT.nodesWithChildren

data Module = Module
  { filePath :: FilePath
  , dependencies :: [Dependency]
  , content :: T.Text
  }

withContent :: Config -> (Dependency, [Dependency]) -> IO Module
withContent Config {tempDir} (Dependency {filePath,fileType}, dependencies) = do
  let name = F.pathToFileName filePath "js"
  rawContent <- fmap T.pack $ readFile $ tempDir </> name
  let content = case fileType of
                  Ast.Elm -> ensureElmIife rawContent
                  _ -> rawContent
  return Module {filePath, dependencies, content}

ensureElmIife :: T.Text -> T.Text
ensureElmIife input =
  "(function() {\n\n" <> input <> "\n\n}.call(exports))"

wrapDependency :: Module -> T.Text
wrapDependency Module {filePath, dependencies, content} =
  wrapModule filePath (foldr replaceRequire content dependencies)

replaceRequire :: Dependency -> T.Text -> T.Text
replaceRequire Dependency {requiredAs, filePath} body =
  T.pack $ subRegex requireRegex (T.unpack body) (T.unpack jetpackRequire)
  where
    fnName = pathToFunctionName filePath "js"
    requireRegex =
      mkRegex $ "require\\([ \t]*['\"]" <> requiredAs <> "['\"][ \t]*\\)"
    jetpackRequire = "jetpackRequire(" <> fnName <> ", \"" <> fnName <> "\")"

writeJsModule :: Config -> [T.Text] -> FilePath -> IO FilePath
writeJsModule Config {outputDir, entryPoints} fns rootFilePath = do
  let out = outputDir </> FP.makeRelative entryPoints rootFilePath
  let rootName = pathToFunctionName rootFilePath "js"
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
    "/* START: foo */\nfunction foo_js(module, exports) {\nconsole.log(42);\n} /* END: foo */\n"
-}
wrapModule :: FilePath -> T.Text -> T.Text
wrapModule path body =
  T.concat
    [ "/* START: "
    , filePath
    , " */"
    , if all isSpace $ T.unpack body
        then "  console.warn(\"" <> filePath <> ": is an empty module!\");"
        else ""
    , "\n"
    , T.concat ["function ", fnName, "(module, exports) {\n"]
    , body
    , "\n} /* END: "
    , filePath
    , " */"
    , "\n"
    ]
  where
    fnName = pathToFunctionName path "js"
    filePath = T.replace "___" "/" $ T.pack path

pathToFunctionName :: FilePath -> String -> T.Text
pathToFunctionName filePath =
  T.replace "@" "_" . T.replace "." "_" . T.pack . F.pathToFileName filePath
