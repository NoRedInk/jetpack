{-# LANGUAGE NamedFieldPuns #-}
module ToolPaths (find, ToolPaths(..)) where

import Config (Config (..))
import Control.Monad ((<=<))
import Control.Monad.Except (throwError)
import Error (Error (BinNotFound))
import System.Directory (makeAbsolute)
import System.Exit
import System.FilePath (FilePath)
import System.Process (system)
import Task

data ToolPaths = ToolPaths
  { elmMake :: FilePath
  , sassc   :: FilePath
  , coffee  :: FilePath
  }

find :: Config -> Task ToolPaths
find Config{elm_make_path, sassc_path, coffee_path} =
  ToolPaths
    <$> (binExists <=< toAbsPathOrBin "elm-make") elm_make_path
    <*> (binExists <=< toAbsPathOrBin "sassc") sassc_path
    <*> (binExists <=< toAbsPathOrBin "coffee") coffee_path

toAbsPathOrBin :: String -> Maybe FilePath -> Task FilePath
toAbsPathOrBin _ (Just pathToBin) = toTask $ makeAbsolute pathToBin
toAbsPathOrBin defaultBin Nothing = return defaultBin

binExists :: String -> Task String
binExists bin = do
  exitCode <- toTask $ system ("which " ++ bin ++ " >/dev/null")
  case exitCode of
    ExitSuccess   -> return bin
    ExitFailure _ -> throwError [BinNotFound bin]
