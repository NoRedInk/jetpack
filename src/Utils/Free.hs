module Utils.Free
  ( toLeft
  , toRight
  ) where

import Control.Monad.Free (Free, hoistFree)
import Data.Functor.Sum (Sum(..))

toLeft
  :: (Functor f, Functor g)
  => Free f a -> Free (Sum f g) a
toLeft = hoistFree InL

toRight
  :: (Functor f, Functor g)
  => Free g a -> Free (Sum f g) a
toRight = hoistFree InR
