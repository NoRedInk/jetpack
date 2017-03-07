module Task
  ( Task
  ) where

import Error
import Control.Monad.Trans.Either

newtype Task = EitherT Error IO
