{-# LANGUAGE DeriveAnyClass #-}

module Hooks
  ( run
  ) where

import Control.Exception.Safe (Exception)
import qualified Control.Exception.Safe as ES
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.IO.Handle
import System.Exit
import System.FilePath ()
import System.Process

-- TODO change to text
run :: String -> IO T.Text
run hookScript = do
  (_, Just out, Just err, ph) <-
    createProcess
      (proc "bash" ["-c", hookScript])
      {std_out = CreatePipe, std_err = CreatePipe, cwd = Nothing}
  ec <- waitForProcess ph
  case ec of
    ExitSuccess -> do
      content <- hGetContents out
      return (T.pack content)
    ExitFailure _ -> do
      content <- hGetContents out
      errContent <- hGetContents err
      ES.throwM $
        HookFailed (T.pack content <> T.pack errContent) (T.pack hookScript)

data Error =
  HookFailed T.Text
             T.Text
  deriving (Typeable, Exception)

instance Show Error where
  show (HookFailed msg hookScript) =
    T.unpack $ T.unlines ["Hook failed!", "", "    $ " <> hookScript, "", msg]
