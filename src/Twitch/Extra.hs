{-# LANGUAGE RecordWildCards #-}

{- this module exists because Twitch does not expose a way to get rid
of "Type anything to quit" and similar prompts using the config
object. This is messing up our progress bars, so we've got to copy a
bunch of unexposed library code in here to remove one line. Blah!

Tracked at https://github.com/jfischoff/twitch/issues/23
-}
module Twitch.Extra where

import Control.Monad
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import System.Directory (canonicalizePath, getCurrentDirectory)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import qualified System.FSNotify as FS
import System.FilePath
       ((</>), addTrailingPathSeparator, isRelative)
import System.IO
       (Handle, IOMode(AppendMode), hClose, hPrint, openFile)
import Twitch
import qualified Twitch.InternalRule as IR

canonicalizeDirPath :: FilePath -> IO FilePath
canonicalizeDirPath path = addTrailingPathSeparator <$> canonicalizePath path

getDirectoryContentsPath :: FilePath -> IO [FilePath]
getDirectoryContentsPath path =
  map (path </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents path

findImmediateDirs :: FilePath -> IO [FilePath]
findImmediateDirs =
  getDirectoryContentsPath >=> filterM doesDirectoryExist >=> canonicalize
  where
    canonicalize :: [FilePath] -> IO [FilePath]
    canonicalize dirs = mapM canonicalizeDirPath dirs

findAllDirs :: FilePath -> IO [FilePath]
findAllDirs path = do
  dirs <- findImmediateDirs path
  nestedDirs <- mapM findAllDirs dirs
  return (dirs ++ concat nestedDirs)

toLogger :: FilePath -> LoggerType -> IO (IR.Issue -> IO (), Maybe Handle)
toLogger filePath lt =
  case lt of
    LogToStdout -> return (print, Nothing)
    LogToFile -> do
      handle <- openFile filePath AppendMode
      return (hPrint handle, Just handle)
    NoLogger -> return (const $ return (), Nothing)

makeAbsolute :: FilePath -> FilePath -> FilePath
makeAbsolute currentDir path =
  if isRelative path
    then currentDir </> path
    else path

toDB :: Real a => a -> DebounceType -> FS.Debounce
toDB amount dbtype =
  case dbtype of
    DebounceDefault -> FS.DebounceDefault
    Debounce -> FS.Debounce $ fromRational $ toRational amount
    NoDebounce -> FS.NoDebounce

optionsToConfig :: Options -> IO (FilePath, IR.Config, Maybe Handle)
optionsToConfig Options {..} = do
  currentDir <- getCurrentDirectory
  let root' = makeAbsolute currentDir $ fromMaybe currentDir root
  -- something about the "log" variable in the original source is not
  -- imported. We don't use a logger, so I just set it explicitly.
  (logger, mhandle) <- toLogger (fromMaybe "log.txt" logFile) NoLogger
  dirsToWatch <-
    if recurseThroughDirectories
      then (root' :) <$> findAllDirs root'
      else return [root']
  let watchConfig =
        FS.WatchConfig
        { FS.confDebounce = toDB debounceAmount debounce
        , FS.confPollInterval = pollInterval
        , FS.confUsePolling = usePolling
        }
  let config =
        IR.Config
        {logger = logger, dirs = dirsToWatch, watchConfig = watchConfig}
  return (root', config, mhandle)

defaultMainWithOptions :: Options -> Dep -> IO ()
defaultMainWithOptions options dep = do
  (root, config, mhandle) <- optionsToConfig options
  manager <- runWithConfig root config dep
  _ <- getLine
  -- THIS IS THE ONLY LINE I CHANGED IN ANY MEANINGFUL WAY AARRRRRGH
  for_ mhandle hClose
  FS.stopManager manager
