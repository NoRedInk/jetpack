module CliArguments
  ( Args (..)
  , defaultArguments
  , readArguments
  ) where

import System.FilePath()
import System.Environment
import Safe (headMay)

data Args = Args
  { entryPointGlob :: Maybe String
  , configPath :: Maybe FilePath }

defaultArguments :: Args
defaultArguments = Args {entryPointGlob = Nothing, configPath = Nothing}

readArguments :: IO Args
readArguments = do
  userGlobArg <- headMay <$> getArgs
  return $ Args
    { configPath = Nothing
    , entryPointGlob = userGlobArg
    }



