{-# OPTIONS_GHC -Wall #-}

module Utils.Files
  ( fileExists
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT, left)
import Errors (Error(..))
import System.Directory (doesFileExist)
import System.FilePath ()

{-| Checks if file exists.
-}
fileExists :: FilePath -> EitherT Error IO ()
fileExists path = do
  exists <- lift $ doesFileExist path
  case exists of
    True -> lift $ return ()
    False -> left $ FileNotFound (show path)
