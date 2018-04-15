module Progress.Counter
  ( mapConcurrently
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as Concurrent
import qualified Data.List as L
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified System.Console.Regions as CR

mapConcurrently :: CR.ConsoleRegion -> T.Text -> (a -> IO b) -> [a] -> IO [b]
mapConcurrently region title go items = do
  let max = T.pack $ show $ L.length items
  mVar <- C.newMVar (0 :: Integer)
  Concurrent.mapConcurrently
    (\item -> do
       result <- go item
       count <- C.takeMVar mVar
       C.putMVar mVar (count + 1)
       _ <-
         CR.setConsoleRegion
           region
           (title <> " (" <> T.pack (show (count + 1)) <> "/" <> max <> ")")
       return result)
    items
