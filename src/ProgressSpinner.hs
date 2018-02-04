module ProgressSpinner
  ( start
  , end
  ) where

import qualified Data.Text as T
import Data.Time.Clock
import System.Console.Questioner.ProgressIndicators as Spinner

start :: T.Text -> IO (ProgressIndicator, UTCTime)
start prompt = do
  p <- Spinner.spinner Spinner.dots1SpinnerTheme (1000 * 200) $ T.unpack prompt
  currentTime <- getCurrentTime
  return (p, currentTime)

end :: (ProgressIndicator, UTCTime) -> T.Text -> IO ()
end (pg, startTime) prompt = do
  currentTime <- getCurrentTime
  let diffTime = diffUTCTime currentTime startTime :: NominalDiffTime
  _ <- Spinner.stopIndicator pg
  let diff = ((fromInteger $ floor $ diffTime * 100) / 100) :: NominalDiffTime
  let diffShow = show $ realToFrac $ toRational diff
  putStrLn $
    "Finished " ++ T.unpack prompt ++ " after   " ++ diffShow ++ " seconds"
