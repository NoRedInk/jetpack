module Task
  ( Task
  ) where

import Control.Monad.Trans.Either
import Error

type Task = EitherT Error IO
