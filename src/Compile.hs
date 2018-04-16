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

import qualified GHC.IO.Handle as IOH
import Parser.Ast as Ast
import System.Clock
       (Clock(Monotonic), TimeSpec, diffTimeSpec, getTime, toNanoSecs)
import qualified System.Console.Regions as CR
import System.Directory (copyFile)
import System.Exit
import System.FilePath ((</>))
import qualified System.Process as P
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

compile :: Args -> Config -> ToolPaths -> Dependency -> IO Result
compile args config toolPaths Dependency {fileType, filePath} =
  runCompiler
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
     Args -> Config -> Ast.SourceType -> ToolPaths -> Arguments -> IO Result
runCompiler args config fileType ToolPaths {elmMake, coffee} arguments@Arguments {input} =
  CR.withConsoleRegion
    CR.Linear
    (\region -> do
       let basemsg =
             ("  " <> fileTypeTitle fileType <> ": " <> T.pack input) :: T.Text
       CR.setConsoleRegion region basemsg
       case fileType of
         Ast.Elm -> elmCompiler elmMake region basemsg args config arguments
         Ast.Js -> jsCompiler region basemsg arguments
         Ast.Coffee -> coffeeCompiler coffee region basemsg args arguments)

fileTypeTitle :: Ast.SourceType -> T.Text
fileTypeTitle fileType =
  case fileType of
    Ast.Elm -> "Elm"
    Ast.Js -> "JS"
    Ast.Coffee -> "Coffee"

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
     FilePath
  -> CR.ConsoleRegion
  -> T.Text
  -> Args
  -> Config
  -> Arguments
  -> IO Result
elmCompiler elmMake region basemsg args Config {elm_root_directory} Arguments { input
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
  runCmd region basemsg args input cmd $ Just elm_root_directory

coffeeCompiler ::
     FilePath -> CR.ConsoleRegion -> T.Text -> Args -> Arguments -> IO Result
coffeeCompiler coffee region basemsg args Arguments {input, output} = do
  let cmd = coffee ++ " -p " ++ input ++ " > " ++ output
  runCmd region basemsg args input cmd Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: CR.ConsoleRegion -> T.Text -> Arguments -> IO Result
jsCompiler _region _basemsg Arguments {input, output} = do
  start <- getTime Monotonic
  _ <- copyFile input output
  -- _ <- tick region basemsg
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
  -> T.Text
  -> Args
  -> FilePath
  -> String
  -> Maybe String
  -> IO Result
runCmd _region _basemsg Args {warn} input cmd maybeCwd = do
  start <- getTime Monotonic
  (ec, errContent, content) <- runAndWaitForProcess cmd maybeCwd
  end <- getTime Monotonic
  case ec of
    ExitSuccess
      -- _ <- tick region basemsg
     -> do
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
    P.createProcess
      (proc "bash" ["-c", cmd])
      {cwd = maybeCwd, std_out = CreatePipe, std_err = CreatePipe}
  ec <- waitForProcess ph
  content <- IOH.hGetContents out
  errContent <- IOH.hGetContents err
  pure (ec, errContent, content)

data Error =
  CompileError T.Text
               T.Text
  deriving (Typeable, Exception)

instance Show Error where
  show (CompileError cmd msg) =
    T.unpack $ T.unlines ["Command:", "", "    $ " <> cmd, "", msg]
