{-| ProgressBar displayes a small progressbar. Running tasks can call `step` to tick the bar one step further.
-}
module ProgressBar
  ( start
  , AP.complete
  , AP.ProgressBar
  , AP.tick
  , pipeAndTick
  ) where

import qualified Data.Text as T
import System.Console.AsciiProgress as AP
import Task (Task, lift)

start :: Int -> T.Text -> Task AP.ProgressBar
start total title =
  lift $
  AP.newProgressBar
    def
    { pgTotal = toInteger total
    , pgOnCompletion =
        Just (T.unpack title ++ " finished after :elapsed seconds")
    , pgCompletedChar = '█'
    , pgPendingChar = '░'
    , pgFormat = T.unpack title ++ " ╢:bar╟ :current/:total"
    }

pipeAndTick :: AP.ProgressBar -> a -> Task a
pipeAndTick pg x = do
  lift $ tick pg
  return x
