module Progress.Region
  ( region
  ) where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified System.Console.Regions as CR

region :: T.Text -> (CR.ConsoleRegion -> IO a) -> IO a
region title run =
  CR.withConsoleRegion
    CR.Linear
    (\region -> do
       CR.setConsoleRegion region title
       result <- run region
       CR.finishConsoleRegion region (title <> " done!")
       return result)
