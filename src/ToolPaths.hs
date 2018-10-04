{-# LANGUAGE DeriveAnyClass #-}

{-| This is used to check if the necessary tools for jetpack exist.
-}
module ToolPaths
  ( find
  , ToolPaths(..)
  ) where

import Config (Config(..))
import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import Control.Monad ((<=<))
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import System.Directory (makeAbsolute)
import System.Exit
import System.FilePath (FilePath)
import System.Process (system)

data ToolPaths = ToolPaths
  { elm :: FilePath
  , coffee :: FilePath
  }

{-| Check if tool from config exists. It falls back to a globally installed bin.
-}
find :: Config -> IO ToolPaths
find Config {elmPath, coffeePath} =
  ToolPaths <$> (binExists <=< toAbsPathOrBin "elm") elmPath <*>
  (binExists <=< toAbsPathOrBin "coffee") coffeePath

toAbsPathOrBin :: String -> Maybe FilePath -> IO FilePath
toAbsPathOrBin _ (Just pathToBin) = makeAbsolute pathToBin
toAbsPathOrBin defaultBin Nothing = return defaultBin

binExists :: String -> IO String
binExists bin = do
  exitCode <- system ("which " ++ bin ++ " >/dev/null")
  case exitCode of
    ExitSuccess -> return bin
    ExitFailure _ -> ES.throwM $ BinNotFound $ T.pack bin

data Error =
  BinNotFound T.Text
  deriving (Typeable, Exception)

instance Show Error where
  show (BinNotFound bin) =
    T.unpack $
    T.unlines
      [ "I had troubles finding the " <> bin <> " command."
      , ""
      , "You might want to install it."
      ]
