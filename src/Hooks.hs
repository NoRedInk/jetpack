{-# LANGUAGE OverloadedStrings #-}
module Hooks
  (run
  ) where

import Control.Monad.Except (throwError)
import qualified Data.Text as T
import Error
import GHC.IO.Handle
import System.Exit
import System.FilePath ()
import System.Process
import Task (Task, toTask)



run :: String -> Task T.Text
run hookScript = do
  (_, Just out, Just err, ph) <- toTask $ createProcess (proc "bash" ["-c", hookScript])
    { std_out = CreatePipe
    , std_err = CreatePipe
    , cwd = Nothing
    }
  ec <- toTask $ waitForProcess ph
  case ec of
      ExitSuccess   -> do
        content <- toTask $ hGetContents out
        return (T.pack $ content)
      ExitFailure _ -> do
        content <- toTask $ hGetContents out
        errContent <- toTask $ hGetContents err
        throwError [HookFailed (content ++ errContent) hookScript]
