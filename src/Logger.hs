{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( Log
  , LogF(..)
  , logger
  , info
  , warn
  , err
  , executor
  ) where

import Control.Monad.Free (Free, liftF)
import Data.Text as T

data LogF a =
  Log Level
      T.Text
      a
  deriving (Functor)

type Log = Free LogF

data Level
  = Warning
  | Info
  | Error
  deriving (Show)

logger :: Level -> T.Text -> Log ()
logger level msg = liftF $ Log level msg ()

warn :: T.Text -> Log ()
warn = logger Warning

info :: T.Text -> Log ()
info = logger Info

err :: T.Text -> Log ()
err = logger Error

executor :: LogF a -> IO ()
executor (Log level msg _) = putStrLn output
  where
    output = show level ++ ": " ++ T.unpack msg
