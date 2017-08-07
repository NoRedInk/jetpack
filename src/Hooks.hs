{-# LANGUAGE OverloadedStrings #-}
module Hooks
  (post
  ) where

import Control.Monad.Except (throwError)
import qualified Data.Text as T
import GHC.IO.Handle
import System.Exit
import System.FilePath ()
import System.Process
import Task (Task, toTask)



post :: FilePath -> Task T.Text
post pathToScript = do
  (_, Just out, _, ph) <- toTask $ createProcess (proc "bash" ["-c", pathToScript])
    { std_out = CreatePipe
    , cwd = Nothing
    }
  ec <- toTask $ waitForProcess ph
  case ec of
      ExitSuccess   -> do
        content <- toTask $ hGetContents out
        return (T.pack content)
      ExitFailure _ -> throwError []
