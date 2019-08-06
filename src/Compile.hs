{-# LANGUAGE DeriveAnyClass #-}

{-|
-}
module Compile where

import Protolude
import CliArguments (Args (..), CompileMode (..))
import Config (Config (Config))
import qualified Config
import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import Dependencies (Dependency (..))
import Formatting (sformat)
import Formatting.Clock (timeSpecs)
import GHC.IO.Handle
import Parser.Ast as Ast
import System.Clock
  ( Clock (Monotonic)
  , TimeSpec
  , diffTimeSpec
  , getTime
  , toNanoSecs
  )
import qualified System.Console.Regions as CR
import System.Directory (copyFile)
import System.FilePath ((</>))
import System.IO (utf8)
import System.Process
import ToolPaths
import Utils.Files (pathToFileName)
import qualified Text.Show
import Data.String

data Result
  = Result
      { duration :: Duration
      , compiledAt :: UTCTime
      , command :: T.Text
      , stdout :: Maybe T.Text
      , compiledFile :: FilePath
      , outputFile :: FilePath
      }
  deriving (Show)

elmFiles :: [Compile.Result] -> [FilePath]
elmFiles [] = []
elmFiles (Result {outputFile} : rest)
  | T.isSuffixOf ".elm.js" (T.pack outputFile) = outputFile : elmFiles rest
  | otherwise = elmFiles rest

printTime :: Args -> Compile.Result -> IO ()
printTime Args {time} Compile.Result {compiledFile, duration} =
  when time $
    TIO.putStrLn $
    T.pack compiledFile <>
    ": " <>
    formatDuration duration

formatDuration :: Duration -> T.Text
formatDuration (Duration start end) = sformat timeSpecs start end

data Duration
  = Duration
      { start :: TimeSpec
      , end :: TimeSpec
      }

instance Show Duration where

  show (Duration start end) =
    show (div (toNanoSecs (diffTimeSpec end start)) 1000000)

compile
  :: CR.ConsoleRegion -> Args -> Config -> ToolPaths -> Dependency -> IO Result
compile
  region
  args
  config@Config {Config.tempDir}
  toolPaths
  Dependency
    { fileType
    , filePath
    } =
    runCompiler
      region
      args
      config
      fileType
      toolPaths
      Arguments
        { input = filePath
        , output = buildArtifactPath tempDir fileType filePath
        }

data Arguments
  = Arguments
      { input :: FilePath
      , output :: FilePath
      }

runCompiler
  :: CR.ConsoleRegion
  -> Args
  -> Config
  -> Ast.SourceType
  -> ToolPaths
  -> Arguments
  -> IO Result
runCompiler region args config fileType ToolPaths {elm, coffee} arguments =
  case fileType of
    Ast.Elm -> elmCompiler elm region args config arguments
    Ast.Js -> jsCompiler region arguments
    Ast.Coffee -> coffeeCompiler coffee region arguments

data Groupped
  = Groupped
      { elm :: [Dependency]
      , coffee :: [Dependency]
      , js :: [Dependency]
      }

group :: [Dependency] -> Groupped
group =
  foldl
    ( \Groupped {elm, js, coffee} dep ->
      case Dependencies.fileType dep of
        Ast.Elm -> Groupped {elm = dep : elm, js, coffee}
        Ast.Js -> Groupped {js = dep : js, elm, coffee}
        Ast.Coffee -> Groupped {coffee = dep : coffee, elm, js}
    )
    Groupped {elm = [], js = [], coffee = []}

buildArtifactPath :: Config.TempDir -> Ast.SourceType -> FilePath -> String
buildArtifactPath tempDir fileType inputPath =
  Config.unTempDir tempDir </> pathToFileName inputPath extension
  where
    extension =
      case fileType of
        Ast.Elm -> "js"
        Ast.Js -> "js"
        Ast.Coffee -> "js"

---------------
-- COMPILERS --
---------------
elmCompiler
  :: Config.ElmPath
  -> CR.ConsoleRegion
  -> Args
  -> Config
  -> Arguments
  -> IO Result
elmCompiler elm region args Config {elmRoot} Arguments {input, output} = do
  let Args {compileMode} = args
  let modeFlag =
        case compileMode of
          Debug -> " --debug"
          Optimize -> " --optimize"
          Normal -> ""
  let cmd =
        T.pack (Config.unElmPath elm) <> " " <> "make" <> " " <> "../" <>
          T.pack input <>
          " --output " <>
          "../" <>
          T.pack output <>
          T.pack modeFlag
  runCmd region input output cmd $ Just $ Config.unElmRoot elmRoot

coffeeCompiler
  :: Config.CoffeePath -> CR.ConsoleRegion -> Arguments -> IO Result
coffeeCompiler coffee region Arguments {input, output} = do
  let cmd =
        T.pack (Config.unCoffeePath coffee) <> " -p " <> T.pack input <> " > " <>
          T.pack output
  runCmd region input output cmd Nothing

{-| The js compiler will basically only copy the file into the tmp dir.
-}
jsCompiler :: CR.ConsoleRegion -> Arguments -> IO Result
jsCompiler region Arguments {input, output} = do
  start <- getTime Monotonic
  _ <- copyFile input output
  CR.setConsoleRegion region $ T.pack input <> " -> " <> T.pack output
  currentTime <- getCurrentTime
  end <- getTime Monotonic
  return
    Result
      { duration = Duration start end
      , compiledAt = currentTime
      , command = T.unwords ["moved", T.pack input, "=>", T.pack output]
      , stdout = Nothing
      , compiledFile = input
      , outputFile = output
      }

runCmd
  :: CR.ConsoleRegion
  -> FilePath
  -> FilePath
  -> T.Text
  -> Maybe String
  -> IO Result
runCmd region input output cmd maybeCwd = do
  CR.setConsoleRegion region $ T.pack input
  start <- getTime Monotonic
  (ec, errContent, content) <- runAndWaitForProcess (T.unpack cmd) maybeCwd
  end <- getTime Monotonic
  case ec of
    ExitSuccess -> do
      CR.setConsoleRegion region $ T.pack "Compiled: " <> T.pack input
      currentTime <- getCurrentTime
      return
        Result
          { duration = Duration start end
          , compiledAt = currentTime
          , command = cmd
          , stdout = Just $ T.pack content
          , compiledFile = input
          , outputFile = output
          }
    ExitFailure _ ->
      ES.throwM $ CompileError cmd (T.pack (content <> errContent))

runAndWaitForProcess :: String -> Maybe String -> IO (ExitCode, String, String)
runAndWaitForProcess cmd maybeCwd = do
  (_, Just out, Just err, ph) <-
    createProcess
      (proc "bash" ["-c", cmd])
        { std_out = CreatePipe
        , std_err = CreatePipe
        , cwd = maybeCwd
        }
  hSetEncoding out utf8
  hSetEncoding err utf8
  gatherOutput ph err out

-- https://passingcuriosity.com/2015/haskell-reading-process-safe-deadlock/
gatherOutput
  :: ProcessHandle -> Handle -> Handle -> IO (ExitCode, String, String)
gatherOutput ph h1 h2 = work mempty mempty
  where
    work acc1 acc2 =
      -- Read any outstanding input.
      do
        bs1 <- BS.hGetNonBlocking h1 (64 * 1024)
        let acc1' = acc1 <> bs1
        bs2 <- BS.hGetNonBlocking h2 (64 * 1024)
        let acc2' = acc2 <> bs2
        -- Check on the process.
        s <- getProcessExitCode ph
        -- Exit or loop.
        case s of
          Nothing -> work acc1' acc2'
          Just ec ->
            -- Get any last bit written between the read and the status
            -- check.
            do
              last1 <- BS.hGetContents h1
              last2 <- BS.hGetContents h2
              pure $ (ec, BSC.unpack $ acc1' <> last1, BSC.unpack $ acc2' <> last2)

data Error
  = CompileError
      T.Text
      T.Text
  deriving (Typeable, Exception)

instance Show Error where

  show (CompileError cmd msg) =
    T.unpack $ T.unlines ["Command:", "", "    $ " <> cmd, "", msg]
