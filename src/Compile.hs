{-|
-}
module Compile where

import CliArguments (Args(..))
import Config (Config(..))
import Control.Monad.Except (throwError)
import Data.List as L
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
import Task (Task, toTask)
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

compile ::
     ProgressBar -> Args -> Config -> ToolPaths -> Dependency -> Task Result
compile pg args config toolPaths Dependency {fileType, filePath} = do
  runCompiler
    pg
    args
    config
    fileType
    toolPaths
    Arguments
    {input = filePath, output = buildArtifactPath args config fileType filePath}

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
    Ast.Coffee -> coffeeCompiler coffee pg args config arguments
    Ast.Sass -> sassCompiler sassc pg args config arguments

buildArtifactPath :: Args -> Config -> Ast.SourceType -> FilePath -> String
buildArtifactPath args config fileType inputPath =
  tmp </> pathToFileName inputPath extension
  where
    tmp = Config.temp_directory config
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
elmCompiler elmMake pg args config Arguments {input, output}
  -- TODO use elm_root_directory instead of the "../" below
  -- also pass in absolute paths
 = do
  let Config {elm_root_directory} = config
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

coffeeCompiler ::
     FilePath -> ProgressBar -> Args -> Config -> Arguments -> Task Result
coffeeCompiler coffee pg args config Arguments {input, output} = do
  let cmd = coffee ++ " -p " ++ input ++ " > " ++ output
  runCmd pg args input cmd Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: ProgressBar -> Arguments -> Task Result
jsCompiler pg Arguments {input, output} =
  toTask $ do
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
sassCompiler sassc pg args config Arguments {input, output} = do
  let Config {sass_load_paths} = config
  let loadPath = L.intercalate ":" sass_load_paths
  let cmd =
        "SASS_PATH=" ++
        loadPath ++ " " ++ sassc ++ " " ++ input ++ " " ++ output
  runCmd pg args input cmd Nothing

runCmd ::
     ProgressBar -> Args -> FilePath -> String -> Maybe String -> Task Result
runCmd pg args input cmd maybeCwd = do
  start <- toTask $ getTime Monotonic
  (ec, errContent, content) <- toTask $ runAndWaitForProcess cmd maybeCwd
  end <- toTask $ getTime Monotonic
  let Args {warn} = args
  case ec of
    ExitSuccess -> do
      _ <- toTask $ tick pg
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
