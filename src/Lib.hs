{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import qualified Config
import Control.Monad.Free (Free, foldFree, liftF, hoistFree)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data.Functor.Sum (Sum(..))
import Data.Text as T
import qualified Error
import qualified Interpreter.Logger as LogI
import qualified Interpreter.Pipeline as PipelineI
import qualified Logger
import Pipeline
import qualified System.Exit
import Task
import Utils.Free (toLeft, toRight)

program :: Pipeline ()
program = do
  args <- readCliArgs
  config <- readConfig (configPath args)
  _ <- compile
  return ()

interpreter :: PipelineF a -> Free (Sum Logger.LogF Task) a
interpreter op =
  toLeft (LogI.interpreter op) *> toRight (lift $ PipelineI.interpreter op)

executor :: Sum Logger.LogF Task a -> Task a
executor (InL log@(Logger.Log _ _ next)) =
  lift $ Logger.executor log >> return next
executor (InR io) = io

run :: IO ()
run = do
  e <- runEitherT $ do foldFree executor $ foldFree interpreter program
  case e of
    Left err -> do
      putStrLn "Compilation failed!"
      System.Exit.die $ Error.description err
    Right _ -> putStrLn "Compilation succeeded!"
