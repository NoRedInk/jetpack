module Task
  ( Task
  ) where

import Control.Monad.Except
import Error

-- TODO we might wanna change this to a `RWST r w s IO a`
type Task = ExceptT [Error] IO
