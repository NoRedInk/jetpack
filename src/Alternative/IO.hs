module Alternative.IO
  ( AlternativeIO
  , tryNext
  , lift
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Except (lift, throwError)

type AlternativeIO = ExceptT () IO

tryNext :: AlternativeIO a
tryNext = throwError ()
