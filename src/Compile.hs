{-|
-}
module Compile where

import CliArguments (Args(..))
import Config (Config(..))
import Control.Monad (when)
import Control.Monad.Except (throwError)
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
import System.Clock
       (Clock(Monotonic), TimeSpec, diffTimeSpec, getTime, toNanoSecs)
import qualified System.Console.Concurrent as CC
import qualified System.Console.Regions as CR
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
  T.unpack $ T.pack compiledFile <> ": " <> T.pack (formatDuration duration)

formatDuration :: Duration -> String
formatDuration (Duration start end) = TL.unpack $ format timeSpecs start end

data Duration = Duration
  { start :: TimeSpec
  , end :: TimeSpec
  }

instance Show Duration where
  show (Duration start end) =
    show (div (toNanoSecs (diffTimeSpec end start)) 1000000)

compile ::
     CR.ConsoleRegion
  -> Args
  -> Config
  -> ToolPaths
  -> Dependency
  -> Task Result
compile region args config toolPaths Dependency {fileType, filePath} =
  runCompiler
    region
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
     CR.ConsoleRegion
  -> Args
  -> Config
  -> Ast.SourceType
  -> ToolPaths
  -> Arguments
  -> Task Result
runCompiler region args config fileType ToolPaths {elmMake, coffee} arguments =
  case fileType of
    Ast.Elm -> elmCompiler elmMake region args config arguments
    Ast.Js -> jsCompiler region arguments
    Ast.Coffee -> coffeeCompiler coffee region args arguments

buildArtifactPath :: Config -> Ast.SourceType -> FilePath -> String
buildArtifactPath Config {temp_directory} fileType inputPath =
  temp_directory </> pathToFileName inputPath extension
  where
    extension =
      case fileType of
        Ast.Elm -> "js"
        Ast.Js -> "js"
        Ast.Coffee -> "js"

---------------
-- COMPILERS --
---------------
elmCompiler ::
     FilePath -> CR.ConsoleRegion -> Args -> Config -> Arguments -> Task Result
elmCompiler elmMake region args Config {elm_root_directory} Arguments { input
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
  runCmd region args input cmd $ Just elm_root_directory

coffeeCompiler ::
     FilePath -> CR.ConsoleRegion -> Args -> Arguments -> Task Result
coffeeCompiler coffee region args Arguments {input, output} = do
  let cmd = coffee ++ " -p " ++ input ++ " > " ++ output
  runCmd region args input cmd Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: CR.ConsoleRegion -> Arguments -> Task Result
jsCompiler region Arguments {input, output} =
  lift $ do
    start <- getTime Monotonic
    _ <- copyFile input output
    _ <- CR.appendConsoleRegion region (T.unpack ".")
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

runCmd ::
     CR.ConsoleRegion
  -> Args
  -> FilePath
  -> String
  -> Maybe String
  -> Task Result
runCmd region Args {warn} input cmd maybeCwd = do
  start <- lift $ getTime Monotonic
  (ec, errContent, content) <- lift $ runAndWaitForProcess cmd maybeCwd
  end <- lift $ getTime Monotonic
  case ec of
    ExitSuccess -> do
      _ <- lift $ CR.appendConsoleRegion region (T.unpack ".")
      currentTime <- lift getCurrentTime
      return
        Result
        { duration = Duration start end
        , compiledAt = currentTime
        , command = T.pack cmd
        , stdout = Just $ T.pack content
        , warnings =
            T.pack <$>
            if warn && errContent /= ""
              then Just errContent
              else Nothing
        , compiledFile = input
        }
    ExitFailure _ ->
      throwError [CompileError (T.pack cmd) (T.pack (content <> errContent))]

runAndWaitForProcess :: String -> Maybe String -> IO (ExitCode, String, String)
runAndWaitForProcess cmd maybeCwd = do
  (_, Just out, Just err, ph) <-
    CC.createProcessConcurrent
      (proc "bash" ["-c", cmd])
      {std_out = CreatePipe, std_err = CreatePipe, cwd = maybeCwd}
  ec <- CC.waitForProcessConcurrent ph
  content <- hGetContents out
  errContent <- hGetContents err
  pure (ec, errContent, content)
