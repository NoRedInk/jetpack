{-# LANGUAGE DeriveAnyClass #-}

{-| This is used to check if the necessary tools for jetpack exist.
-}
module ToolPaths
  ( find
  , ToolPaths(..)
  ) where

import Config (Config(Config))
import qualified Config
import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import System.Directory (makeAbsolute)
import System.Exit
import System.FilePath (FilePath)
import System.Process (system)

data ToolPaths = ToolPaths
  { elm :: Config.ElmPath
  , coffee :: Config.CoffeePath
  }

{-| Check if tool from config exists. It falls back to a globally installed bin.
-}
find :: Config -> IO ToolPaths
find Config {Config.elmPath, Config.coffeePath} = do
  elmPath' <- toAbsPathOrBin "elm" (Config.unElmPath <$> elmPath)
  _ <- binExists elmPath'
  let elm = Config.ElmPath elmPath'
  coffeePath' <- toAbsPathOrBin "coffee" (Config.unCoffeePath <$> coffeePath)
  _ <- binExists coffeePath'
  let coffee = Config.CoffeePath coffeePath'
  pure $ ToolPaths {elm, coffee}

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
