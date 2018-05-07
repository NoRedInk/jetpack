{-| ProgressBar displayes a small progressbar. Running tasks can call `step` to tick the bar one step further.
-}
module ProgressBar
  ( start
  , ProgressBar
  , tick
  , pipeAndTick
  , complete
  ) where

import qualified Data.Text as T

newtype ProgressBar =
  ProgressBar ()

start :: Int -> T.Text -> IO ProgressBar
start _ _ = return (ProgressBar ())

pipeAndTick :: ProgressBar -> a -> IO a
pipeAndTick _ x = return x

complete :: ProgressBar -> IO ProgressBar
complete _ = return (ProgressBar ())

tick :: ProgressBar -> IO ProgressBar
tick _ = return (ProgressBar ())
