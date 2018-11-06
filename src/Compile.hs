{-# LANGUAGE DeriveAnyClass #-}

{-|
-}
module Compile where

import CliArguments (Args(..))
import Config (Config(..))
import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import Control.Monad (when)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import Dependencies (Dependency(..))
import Formatting (sformat)
import Formatting.Clock (timeSpecs)
import GHC.IO.Handle
import Parser.Ast as Ast
import ProgressBar (ProgressBar, tick)
import System.Clock
       (Clock(Monotonic), TimeSpec, diffTimeSpec, getTime, toNanoSecs)
import System.Directory (copyFile)
import System.Exit
import System.FilePath ((</>))
import System.Process
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
  TIO.putStrLn $ T.pack compiledFile <> ": " <> formatDuration duration

formatDuration :: Duration -> T.Text
formatDuration (Duration start end) = sformat timeSpecs start end

data Duration = Duration
  { start :: TimeSpec
  , end :: TimeSpec
  }

instance Show Duration where
  show (Duration start end) =
    show (div (toNanoSecs (diffTimeSpec end start)) 1000000)

compile :: ProgressBar -> Args -> Config -> ToolPaths -> Dependency -> IO Result
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
  -> IO Result
runCompiler pg args config fileType ToolPaths {elmMake, coffee} arguments =
  case fileType of
    Ast.Elm -> elmCompiler elmMake pg args config arguments
    Ast.Js -> jsCompiler pg arguments
    Ast.Coffee -> coffeeCompiler coffee pg args arguments

buildArtifactPath :: Config -> Ast.SourceType -> FilePath -> String
buildArtifactPath Config {tempDir} fileType inputPath =
  tempDir </> pathToFileName inputPath extension
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
     FilePath -> ProgressBar -> Args -> Config -> Arguments -> IO Result
elmCompiler elmMake pg args Config {elmRoot} Arguments {input, output} = do
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
  runCmd pg args input cmd $ Just elmRoot

coffeeCompiler :: FilePath -> ProgressBar -> Args -> Arguments -> IO Result
coffeeCompiler coffee pg args Arguments {input, output} = do
  let cmd = coffee ++ " -p " ++ input ++ " > " ++ output
  runCmd pg args input cmd Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: ProgressBar -> Arguments -> IO Result
jsCompiler pg Arguments {input, output} = do
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

runCmd :: ProgressBar -> Args -> FilePath -> String -> Maybe String -> IO Result
runCmd pg Args {warn} input cmd maybeCwd = do
  start <- getTime Monotonic
  (ec, errContent, content) <- runAndWaitForProcess cmd maybeCwd
  end <- getTime Monotonic
  case ec of
    ExitSuccess -> do
      _ <- tick pg
      currentTime <- getCurrentTime
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
      ES.throwM $ CompileError (T.pack cmd) (T.pack (content <> errContent))

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

data Error =
  CompileError T.Text
               T.Text
  deriving (Typeable, Exception)

instance Show Error where
  show (CompileError cmd msg) =
    T.unpack $ T.unlines ["Command:", "", "    $ " <> cmd, "", msg]
