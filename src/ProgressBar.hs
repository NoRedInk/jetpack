{-| ProgressBar displayes a small progressbar. Running tasks can call `step` to tick the bar one step further.
-}
module ProgressBar
  ( start
  , complete
  , ProgressBar
  , tick
  ) where

import qualified Data.Text as T

newtype ProgressBar =
  ProgressBar ()

start :: Int -> T.Text -> IO ProgressBar
start _ _ = return (ProgressBar ())

complete :: ProgressBar -> IO ()
complete _ = return ()

tick :: ProgressBar -> IO ()
tick _ = return ()
