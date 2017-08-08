{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgressSpinner
  ( start
  , end
  ) where

import Control.Monad.State (get, modify)
import qualified Data.Text as T
import Env
import System.Console.Questioner.ProgressIndicators as Spinner
import Task (Task, toTask)

start :: T.Text -> Task ()
start prompt = do
  p <- toTask $ Spinner.spinner Spinner.dots1SpinnerTheme (1000 * 200) $ T.unpack prompt
  modify (\env -> env { progressSpinner = Just p })
  return ()

end :: Task ()
end = do
  Env {progressSpinner} <- get
  case progressSpinner of
    Just pg -> toTask $ Spinner.stopIndicator pg
    Nothing -> return ()
