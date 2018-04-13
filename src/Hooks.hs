module Hooks
  ( run
  ) where

import Control.Monad.Except (throwError)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Error
import GHC.IO.Handle
import qualified System.Console.Concurrent as CC
import System.Exit
import System.FilePath ()
import System.Process
import Task (Task, lift)

-- TODO change to text
run :: String -> Task T.Text
run hookScript = do
  (_, Just out, Just err, ph) <-
    lift $
    CC.createProcessConcurrent
      (proc "bash" ["-c", hookScript])
      {std_out = CreatePipe, std_err = CreatePipe, cwd = Nothing}
  ec <- lift $ CC.waitForProcessConcurrent ph
  case ec of
    ExitSuccess -> do
      content <- lift $ hGetContents out
      return (T.pack content)
    ExitFailure _ -> do
      content <- lift $ hGetContents out
      errContent <- lift $ hGetContents err
      throwError
        [HookFailed (T.pack content <> T.pack errContent) (T.pack hookScript)]
