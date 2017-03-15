{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

module Init where

import Config
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Error (Error (BinNotFound))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit
import System.FilePath
import System.Process (system)
import Task (Task)

requiredBins :: [String]
requiredBins = ["coffee", "sassc", "elm-make"]

setup :: Config -> Task ()
setup Config { temp_directory } =  do
  _ <- traverse binExists requiredBins
  _ <- lift $ createDirectoryIfMissing True temp_directory
  let depsJSONPath = temp_directory </> "deps" <.> "json"
  exists <- lift $ doesFileExist depsJSONPath
  if exists
     then return ()
     else lift $ writeFile depsJSONPath "[]"

binExists :: String -> Task ()
binExists bin = do
  exitCode <- lift $ system ("which " ++ bin)
  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure _ -> throwError [BinNotFound bin]
