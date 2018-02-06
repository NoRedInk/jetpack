{-| Concat all modules required in an entrypoint into one file.
-}
module ConcatModule where

import Config

import qualified Data.List.Utils as LU
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Tree as Tree
import Dependencies
import qualified Parser.Ast as Ast
import ProgressBar (ProgressBar, tick)
import System.Directory (createDirectoryIfMissing)
import System.FilePath as FP
import Task
import Text.Regex (mkRegex, subRegex)
import qualified Utils.Files as F
import qualified Utils.Tree as UT

wrap :: ProgressBar -> Config -> DependencyTree -> Task FilePath
wrap pg env dep = do
  wrapped <- traverse (wrapper env) $ uniqNodes dep
  out <- writeModule env dep $ catMaybes wrapped
  _ <- lift $ tick pg
  return out

uniqNodes :: DependencyTree -> [(Dependency, [Dependency])]
uniqNodes = LU.uniq . UT.nodesWithChildren

wrapper :: Config -> (Dependency, [Dependency]) -> Task (Maybe T.Text)
wrapper config (d@Dependency {filePath}, ds) = do
  let Config {temp_directory} = config
  if compilesToJs d
    then lift $ do
           let name = F.pathToFileName filePath "js"
           content <- readFile $ temp_directory </> name
           let fnName = F.pathToFunctionName filePath "js"
           let replacedContent = foldr replaceRequire (T.pack content) ds
           let wrapped = wrapModule fnName replacedContent
           return $ Just wrapped
    else return Nothing

-- TODO check if require got replaced
replaceRequire :: Dependency -> T.Text -> T.Text
replaceRequire Dependency {requiredAs, filePath} body =
  T.pack $ subRegex requireRegex (T.unpack body) jetpackRequire
  where
    fnName = T.unpack $ F.pathToFunctionName filePath "js"
    requireRegex =
      mkRegex $ "require\\([ \t]*['\"]" ++ requiredAs ++ "['\"][ \t]*\\)"
    jetpackRequire = "jetpackRequire(" ++ fnName ++ ", \"" ++ fnName ++ "\")"

compilesToJs :: Dependency -> Bool
compilesToJs Dependency {filePath, fileType} =
  case fileType of
    Ast.Js -> FP.takeExtension filePath /= ".css"
    Ast.Elm -> True
    Ast.Coffee -> True
    _ -> False

writeModule :: Config -> DependencyTree -> [T.Text] -> Task FilePath
writeModule config dependencyTree fns = do
  let root@Dependency {filePath} = Tree.rootLabel dependencyTree
  if compilesToJs root
    then writeJsModule config filePath fns
    else writeCssModule config filePath $
         UT.roots $ Tree.subForest dependencyTree

writeJsModule :: Config -> FilePath -> [T.Text] -> Task FilePath
writeJsModule Config {output_js_directory, entry_points} rootFilePath fns =
  lift $ do
    let out =
          outputPath
            Output
            { outDir = output_js_directory
            , moduleDir = entry_points
            , name = rootFilePath
            }
    let rootName = F.pathToFunctionName rootFilePath "js"
    createDirectoryIfMissing True $ FP.takeDirectory out
    writeFile out $ T.unpack $ addBoilerplate rootName fns
    return out

writeCssModule :: Config -> FilePath -> [Dependency] -> Task FilePath
writeCssModule Config {output_css_directory, entry_points, temp_directory} rootFilePath deps =
  lift $ do
    let out =
          outputPath
            Output
            { outDir = output_css_directory
            , moduleDir = entry_points
            , name = rootFilePath
            }
    createDirectoryIfMissing True $ FP.takeDirectory out
    let cssPaths =
          fmap
            ((</>) temp_directory . flip F.pathToFileName "css" . filePath)
            deps
    css <- traverse readFile cssPaths
    writeFile out $ T.unpack $ T.unlines $ fmap T.pack css
    return out

data Output = Output
  { outDir :: FilePath
  , moduleDir :: FilePath
  , name :: FilePath
  }

outputPath :: Output -> FilePath
outputPath Output {outDir, moduleDir, name} =
  outDir </> FP.makeRelative moduleDir name

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
wrapModule _ "" = ""
wrapModule fnName body =
  T.concat
    [ "/* START: "
    , fnName
    , " */"
    , "\n"
    , T.concat ["function ", fnName, "(module, exports) {\n"]
    , body
    , "\n} /* END: "
    , fnName
    , " */"
    , "\n"
    ]
