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
import Formatting ((%), format)
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

data Duration = Duration
  { start :: TimeSpec
  , end :: TimeSpec
  }

data Result = Result
  { duration :: Duration
  , compiledAt :: UTCTime
  , command :: T.Text
  , stdout :: Maybe T.Text
  , warnings :: Maybe T.Text
  , compiledFile :: FilePath
  } deriving (Show)

instance Show Duration where
  show (Duration start end) = TL.unpack $ format timeSpecs start end

newtype Compiler = Compiler
  { runCompiler :: FilePath -> FilePath -> Task Result
  }

compile :: ToolPaths -> Dependency -> Task Result
compile toolPaths Dependency {fileType, filePath} = do
  config <- Task.getConfig
  let (c, outputType) = compiler fileType config toolPaths
  let outputPath = buildArtifactPath config outputType filePath
  runCompiler c filePath outputPath

compiler :: Ast.SourceType -> Config -> ToolPaths -> (Compiler, String)
compiler fileType config ToolPaths {elmMake, sassc, coffee} =
  case fileType of
    Ast.Elm -> (elmCompiler config elmMake, "js")
    Ast.Js -> (jsCompiler, "js")
    Ast.Coffee -> (coffeeCompiler coffee, "js")
    Ast.Sass -> (sassCompiler config sassc, "css")

buildArtifactPath :: Config -> String -> FilePath -> String
buildArtifactPath Config {temp_directory} extension inputPath =
  temp_directory </> pathToFileName inputPath extension

---------------
-- COMPILERS --
---------------
elmCompiler :: Config -> FilePath -> Compiler
elmCompiler Config {elm_root_directory} elmMake =
  Compiler $ \input output
  -- TODO use elm_root_directory instead of the "../" below
  -- also pass in absolute paths
   -> do
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

coffeeCompiler :: FilePath -> Compiler
coffeeCompiler coffee =
  Compiler $ \input output -> do
    let cmd = coffee ++ " -p " ++ input ++ " > " ++ output
    runCmd input cmd Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: Compiler
jsCompiler =
  Compiler $ \input output -> do
    start <- toTask $ getTime Monotonic
    _ <- toTask $ copyFile input output
    _ <- ProgressBar.step
    currentTime <- toTask getCurrentTime
    end <- toTask $ getTime Monotonic
    return $
      Result
      { duration = Duration start end
      , compiledAt = currentTime
      , command = T.unwords ["moved", T.pack input, "=>", T.pack output]
      , stdout = Nothing
      , warnings = Nothing
      , compiledFile = input
      }

sassCompiler :: Config -> FilePath -> Compiler
sassCompiler Config {sass_load_paths} sassc =
  Compiler $ \input output -> do
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
      currentTime <- toTask $ getCurrentTime
      return $
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
    ExitFailure _ -> do
      throwError [CompileError cmd (content ++ errContent)]

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
