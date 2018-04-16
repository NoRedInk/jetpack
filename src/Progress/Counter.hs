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
import qualified Progress.Loader
import qualified System.Console.Regions as CR

mapConcurrently :: CR.ConsoleRegion -> (a -> IO b) -> [a] -> IO [b]
mapConcurrently region go items = do
  withLoader region $ \r -> do
    let max = T.pack $ show $ L.length items
    mVar <- C.newMVar (0 :: Integer)
    Concurrent.mapConcurrently
      (\item -> do
         result <- go item
         count <- C.takeMVar mVar
         C.putMVar mVar (count + 1)
         _ <-
           CR.setConsoleRegion
             r
             ("(" <> T.pack (show (count + 1)) <> "/" <> max <> ")")
         return result)
      items

mapGroupConcurrently ::
     Ord b => CR.ConsoleRegion -> (a -> b) -> (a -> IO c) -> [a] -> IO [c]
mapGroupConcurrently region groupper go items =
  withLoader region $ \r -> do
    let max = T.pack $ show $ L.length items
    let groupped = groupWith groupper items
    mVar <- C.newMVar (0 :: Integer)
    result <-
      Concurrent.mapConcurrently
        (T.traverse $ \item -> do
           result <- go item
           count <- C.takeMVar mVar
           C.putMVar mVar (count + 1)
           _ <-
             CR.setConsoleRegion
               r
               ("(" <> T.pack (show (count + 1)) <> "/" <> max <> ")")
           return result)
        groupped
    return (mconcat result)

withLoader :: CR.ConsoleRegion -> (CR.ConsoleRegion -> IO a) -> IO a
withLoader region go =
  CR.withConsoleRegion (CR.InLine region) $ \first -> do
    loader <- Progress.Loader.start first
    result <- CR.withConsoleRegion (CR.InLine first) go
    _ <- Progress.Loader.stop loader
    return result
