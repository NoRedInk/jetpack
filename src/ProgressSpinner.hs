module ProgressSpinner
  ( start
  , end
  ) where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Formatting (format)
import Formatting.Clock (timeSpecs)
import System.Clock (Clock(Monotonic), TimeSpec, getTime)
import System.Console.Questioner.ProgressIndicators as Spinner

start :: T.Text -> IO (ProgressIndicator, TimeSpec)
start prompt = do
  p <- Spinner.spinner Spinner.dots1SpinnerTheme (1000 * 200) $ T.unpack prompt
  start <- getTime Monotonic
  return (p, start)

end :: (ProgressIndicator, TimeSpec) -> T.Text -> IO ()
end (pg, start) prompt = do
  end <- getTime Monotonic
  _ <- Spinner.stopIndicator pg
  let diffShow = T.pack $ TL.unpack $ format timeSpecs start end
  TIO.putStrLn $ "Finished " <> prompt <> " after   " <> diffShow
