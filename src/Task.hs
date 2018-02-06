{-| Task is an enhanced IO, it holds state and allows to stop all computation by throwing an error.
-}
module Task
  ( Task
  , lift
  , runExceptT
  ) where

import Control.Monad.Except
import Error

type Task = ExceptT [Error] IO
