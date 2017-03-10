module Task
  ( Task
  ) where

import Control.Monad.Trans.Either
import Error

-- TODO we might wanna change this to a `RWST r w s IO a`
type Task = EitherT [Error] IO
