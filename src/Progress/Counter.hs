module Progress.Counter
  ( mapConcurrently
  , mapGroupConcurrently
  ) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as Concurrent
import qualified Data.List as L
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Traversable as T
import GHC.Exts (groupWith)
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

mapGroupConcurrently ::
     Ord b
  => CR.ConsoleRegion
  -> T.Text
  -> (a -> b)
  -> (a -> IO c)
  -> [a]
  -> IO [c]
mapGroupConcurrently region title groupper go items = do
  let max = T.pack $ show $ L.length items
  let groupped = groupWith groupper items
  mVar <- C.newMVar (0 :: Integer)
  result <-
    Concurrent.mapConcurrently
      (T.traverse
         (\item -> do
            result <- go item
            count <- C.takeMVar mVar
            C.putMVar mVar (count + 1)
            _ <-
              CR.setConsoleRegion
                region
                (title <> " (" <> T.pack (show (count + 1)) <> "/" <> max <> ")")
            return result))
      groupped
  return (mconcat result)
