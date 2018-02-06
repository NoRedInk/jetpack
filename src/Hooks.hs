module Hooks
  ( run
  ) where

import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Error
import GHC.IO.Handle
import System.Exit
import System.FilePath ()
import System.Process
import Task (Task, lift)

run :: String -> Task T.Text
run hookScript = do
  (_, Just out, Just err, ph) <-
    lift $
    createProcess
      (proc "bash" ["-c", hookScript])
      {std_out = CreatePipe, std_err = CreatePipe, cwd = Nothing}
  ec <- lift $ waitForProcess ph
  case ec of
    ExitSuccess -> do
      content <- lift $ hGetContents out
      return (T.pack content)
    ExitFailure _ -> do
      content <- lift $ hGetContents out
      errContent <- lift $ hGetContents err
      throwError [HookFailed (content ++ errContent) hookScript]
