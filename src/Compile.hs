{-|
-}
module Compile where

import CliArguments (Args(..))
import Config (Config(..))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.List as L
import Data.Semigroup ((<>))
import Data.Text as T
import Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime, getCurrentTime)
import Dependencies (Dependency(..))
import Error
import Formatting (format)
import Formatting.Clock (timeSpecs)
import GHC.IO.Handle
import Parser.Ast as Ast
import ProgressBar (ProgressBar, tick)
import System.Clock (Clock(Monotonic), TimeSpec, getTime)
import System.Directory (copyFile)
import System.Exit
import System.FilePath ((</>))
import System.Process
import Task (Task, lift)
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

printTime :: Args -> Compile.Result -> IO ()
printTime Args {time} Compile.Result {compiledFile, duration} =
  when time $
  putStrLn $
  T.unpack $ (T.pack compiledFile) <> ": " <> (T.pack $ show duration)

data Duration = Duration
  { start :: TimeSpec
  , end :: TimeSpec
  }

instance Show Duration where
  show (Duration start end) = TL.unpack $ format timeSpecs start end

compile ::
     ProgressBar -> Args -> Config -> ToolPaths -> Dependency -> Task Result
compile pg args config toolPaths Dependency {fileType, filePath} =
  runCompiler
    pg
    args
    config
    fileType
    toolPaths
    Arguments
    {input = filePath, output = buildArtifactPath config fileType filePath}

data Arguments = Arguments
  { input :: FilePath
  , output :: FilePath
  }

runCompiler ::
     ProgressBar
  -> Args
  -> Config
  -> Ast.SourceType
  -> ToolPaths
  -> Arguments
  -> Task Result
runCompiler pg args config fileType ToolPaths {elmMake, sassc, coffee} arguments =
  case fileType of
    Ast.Elm -> elmCompiler elmMake pg args config arguments
    Ast.Js -> jsCompiler pg arguments
    Ast.Coffee -> coffeeCompiler coffee pg args arguments
    Ast.Sass -> sassCompiler sassc pg args config arguments

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
elmCompiler ::
     FilePath -> ProgressBar -> Args -> Config -> Arguments -> Task Result
elmCompiler elmMake pg args Config {elm_root_directory} Arguments { input
                                                                  , output
                                                                  } = do
  let Args {debug, warn} = args
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
  runCmd pg args input cmd $ Just elm_root_directory

coffeeCompiler :: FilePath -> ProgressBar -> Args -> Arguments -> Task Result
coffeeCompiler coffee pg args Arguments {input, output} = do
  let cmd = coffee ++ " -p " ++ input ++ " > " ++ output
  runCmd pg args input cmd Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: ProgressBar -> Arguments -> Task Result
jsCompiler pg Arguments {input, output} =
  lift $ do
    start <- getTime Monotonic
    _ <- copyFile input output
    _ <- tick pg
    currentTime <- getCurrentTime
    end <- getTime Monotonic
    return
      Result
      { duration = Duration start end
      , compiledAt = currentTime
      , command = T.unwords ["moved", T.pack input, "=>", T.pack output]
      , stdout = Nothing
      , warnings = Nothing
      , compiledFile = input
      }

sassCompiler ::
     FilePath -> ProgressBar -> Args -> Config -> Arguments -> Task Result
sassCompiler sassc pg args Config {sass_load_paths} Arguments {input, output} = do
  let loadPath = L.intercalate ":" sass_load_paths
  let cmd =
        "SASS_PATH=" ++
        loadPath ++ " " ++ sassc ++ " " ++ input ++ " " ++ output
  runCmd pg args input cmd Nothing

runCmd ::
     ProgressBar -> Args -> FilePath -> String -> Maybe String -> Task Result
runCmd pg Args {warn} input cmd maybeCwd = do
  start <- lift $ getTime Monotonic
  (ec, errContent, content) <- lift $ runAndWaitForProcess cmd maybeCwd
  end <- lift $ getTime Monotonic
  case ec of
    ExitSuccess -> do
      _ <- lift $ tick pg
      currentTime <- lift getCurrentTime
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
