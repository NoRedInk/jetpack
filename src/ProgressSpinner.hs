{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module ProgressSpinner
  ( start
  , end
  ) where

import Control.Monad.State (get, modify)
import qualified Data.Text as T
import Data.Time.Clock
import Env
import System.Console.Questioner.ProgressIndicators as Spinner
import Task (Task, toTask)


start :: T.Text -> Task ()
start prompt = do
  p <- toTask $ Spinner.spinner Spinner.dots1SpinnerTheme (1000 * 200) $ T.unpack prompt
  currentTime <- toTask getCurrentTime
  modify (\env -> env { progressSpinner = Just (p, currentTime) })
  return ()


end :: T.Text -> Task ()
end prompt = do
  Env {progressSpinner} <- get
  case progressSpinner of
    Nothing -> return ()
    Just (pg, startTime) -> toTask $ do
      currentTime <- getCurrentTime
      let diffTime = diffUTCTime currentTime startTime :: NominalDiffTime
      _ <- Spinner.stopIndicator pg
      let diff = ((fromInteger $ floor $ diffTime * 100) / 100) :: NominalDiffTime
      let diffShow = realToFrac $ toRational diff
      putStrLn $ "Finished " ++ T.unpack prompt ++ " after   " ++ (show diffShow ++ " seconds")
