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
import Utils.Free (toLeft, toRight)

program :: Pipeline ()
program = do
  args <- readCliArgs
  config <- readConfig (configPath args)
  _ <- compile
  return ()

interpreter :: PipelineF a -> Free (Sum Logger.LogF RunnableF) a
interpreter op =
  toLeft (LogI.interpreter op) *> toRight (PipelineI.interpreter op)

executor :: Sum Logger.LogF RunnableF a -> (IO a)
executor (InL log@(Logger.Log _ _ next)) = Logger.executor log >> return next
executor (InR (Run io next)) = io >> return next

run :: IO ()
run = do
  e <- runEitherT $ lift $ do foldFree executor $ foldFree interpreter program
  case e of
    Left err -> do
      putStrLn $ Error.description err
      System.Exit.die $ Error.description err
    Right _ -> putStrLn "Compilation succeeded!"
