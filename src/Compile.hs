{-|
-}
module Compile where

import Config (Config(..))
import Control.Monad.Except (throwError)
import Data.List as L
import Data.Text as T
import Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime, getCurrentTime)
import Dependencies (Dependency(..))
import Env
import Error
import Formatting (format)
import Formatting.Clock (timeSpecs)
import GHC.IO.Handle
import Parser.Ast as Ast
import qualified ProgressBar
import System.Clock (Clock(Monotonic), TimeSpec, getTime)
import System.Directory (copyFile)
import System.Exit
import System.FilePath ((</>))
import System.Process
import Task (Task, getArgs, getConfig, toTask)
import ToolPaths
import Utils.Files (pathToFileName)

data Result = Result
  { duration :: Duration
  , compiledAt :: UTCTime
  , command :: T.Text
  , stdout :: Maybe T.Text
  , warnings :: Maybe T.Text
  , compiledFile :: FilePath
  } deriving (Show)

data Duration = Duration
  { start :: TimeSpec
  , end :: TimeSpec
  }

instance Show Duration where
  show (Duration start end) = TL.unpack $ format timeSpecs start end

compile :: ToolPaths -> Dependency -> Task Result
compile toolPaths Dependency {fileType, filePath} = do
  config <- Task.getConfig
  runCompiler
    config
    fileType
    toolPaths
    Arguments
    {input = filePath, output = buildArtifactPath config fileType filePath}

data Arguments = Arguments
  { input :: FilePath
  , output :: FilePath
  }

runCompiler :: Config -> Ast.SourceType -> ToolPaths -> Arguments -> Task Result
runCompiler config fileType ToolPaths {elmMake, sassc, coffee} arguments =
  case fileType of
    Ast.Elm -> elmCompiler elmMake config arguments
    Ast.Js -> jsCompiler arguments
    Ast.Coffee -> coffeeCompiler coffee arguments
    Ast.Sass -> sassCompiler sassc config arguments

buildArtifactPath :: Config -> Ast.SourceType -> FilePath -> String
buildArtifactPath Config {temp_directory} fileType inputPath =
  temp_directory </> pathToFileName inputPath extension
  where
    extension =
      case fileType of
        Ast.Elm -> "js"
        Ast.Js -> "js"
        Ast.Coffee -> "js"
        Ast.Sass -> "css"

---------------
-- COMPILERS --
---------------
elmCompiler :: FilePath -> Config -> Arguments -> Task Result
elmCompiler elmMake Config {elm_root_directory} Arguments {input, output}
  -- TODO use elm_root_directory instead of the "../" below
  -- also pass in absolute paths
 = do
  Args {debug, warn} <- Task.getArgs
  let debugFlag =
        if debug
          then " --debug"
          else ""
  let warnFlag =
        if warn
          then " --warn"
          else ""
  let cmd =
        elmMake ++
        " " ++
        "../" ++
        input ++
        " --output " ++ "../" ++ output ++ debugFlag ++ " --yes" ++ warnFlag
  runCmd input cmd $ Just elm_root_directory

coffeeCompiler :: FilePath -> Arguments -> Task Result
coffeeCompiler coffee Arguments {input, output} = do
  let cmd = coffee ++ " -p " ++ input ++ " > " ++ output
  runCmd input cmd Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: Arguments -> Task Result
jsCompiler Arguments {input, output} = do
  start <- toTask $ getTime Monotonic
  _ <- toTask $ copyFile input output
  _ <- ProgressBar.step
  currentTime <- toTask getCurrentTime
  end <- toTask $ getTime Monotonic
  return
    Result
    { duration = Duration start end
    , compiledAt = currentTime
    , command = T.unwords ["moved", T.pack input, "=>", T.pack output]
    , stdout = Nothing
    , warnings = Nothing
    , compiledFile = input
    }

sassCompiler :: FilePath -> Config -> Arguments -> Task Result
sassCompiler sassc Config {sass_load_paths} Arguments {input, output} = do
  let loadPath = L.intercalate ":" sass_load_paths
  let cmd =
        "SASS_PATH=" ++
        loadPath ++ " " ++ sassc ++ " " ++ input ++ " " ++ output
  runCmd input cmd Nothing

runCmd :: FilePath -> String -> Maybe String -> Task Result
runCmd input cmd maybeCwd = do
  start <- toTask $ getTime Monotonic
  (ec, errContent, content) <- toTask $ runAndWaitForProcess cmd maybeCwd
  end <- toTask $ getTime Monotonic
  Args {warn} <- Task.getArgs
  case ec of
    ExitSuccess -> do
      _ <- ProgressBar.step
      currentTime <- toTask getCurrentTime
      return
        Result
        { duration = Duration start end
        , compiledAt = currentTime
        , command = (T.pack cmd)
        , stdout = (Just $ T.pack content)
        , warnings =
            T.pack <$>
            if warn && errContent /= ""
              then Just errContent
              else Nothing
        , compiledFile = input
        }
    ExitFailure _ -> throwError [CompileError cmd (content ++ errContent)]

runAndWaitForProcess :: String -> Maybe String -> IO (ExitCode, String, String)
runAndWaitForProcess cmd maybeCwd = do
  (_, Just out, Just err, ph) <-
    createProcess
      (proc "bash" ["-c", cmd])
      {std_out = CreatePipe, std_err = CreatePipe, cwd = maybeCwd}
  ec <- waitForProcess ph
  content <- hGetContents out
  errContent <- hGetContents err
  pure (ec, errContent, content)
