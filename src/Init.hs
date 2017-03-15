{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

{-| Setup working dir for jetpack.
-}
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
  createDepsJsonIfMissing temp_directory

createDepsJsonIfMissing :: FilePath -> Task ()
createDepsJsonIfMissing tempDirectory = lift $ do
  let depsJSONPath = tempDirectory </> "deps" <.> "json"
  exists <- doesFileExist depsJSONPath
  if exists
    then return ()
    else writeFile depsJSONPath "[]"

binExists :: String -> Task ()
binExists bin = do
  exitCode <- lift $ system ("which " ++ bin)
  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure _ -> throwError [BinNotFound bin]
