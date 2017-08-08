{-# LANGUAGE OverloadedStrings #-}
module Hooks
  (run
  ) where

import Control.Monad.Except (throwError)
import qualified Data.Text as T
import GHC.IO.Handle
import System.Exit
import System.FilePath ()
import System.Process
import Task (Task, toTask)



run :: FilePath -> Task T.Text
run pathToScript = do
  (_, Just out, Just err, ph) <- toTask $ createProcess (proc "bash" ["-c", pathToScript])
    { std_out = CreatePipe
    , std_err = CreatePipe
    , cwd = Nothing
    }
  ec <- toTask $ waitForProcess ph
  case ec of
      ExitSuccess   -> do
        content <- toTask $ hGetContents out
        errContent <- toTask $ hGetContents err
        return (T.pack $ content ++ errContent)
      ExitFailure _ -> throwError []
