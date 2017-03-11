{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import Config ()
import Control.Monad.Except
import Control.Monad.Free (Free, foldFree)
import Control.Monad.Trans.Class
import Data.Functor.Sum (Sum (..))
import Data.List as L
import Data.List.Utils (uniq)
import Data.Tree as Tree
import qualified Error
import qualified Interpreter.Logger as LogI
import qualified Interpreter.Pipeline as PipelineI
import qualified Logger
import Pipeline
import qualified System.Exit
import Task
import Utils.Free (toLeft, toRight)

run :: IO ()
run = do
  e <- runExceptT $ do runProgram program
  case e of
    Left err -> do
      putStrLn "Compilation failed!"
      System.Exit.die $ L.unlines $ fmap Error.description err
    Right _ -> putStrLn "Compilation succeeded!"

program :: Pipeline ()
program = do
  args <- readCliArgs -- TODO we propably want to read cli args before running the program.
  config <- readConfig (configPath args)
  deps <- dependencies config
  let modules = uniq $ concatMap Tree.flatten deps
  _ <- compile modules
  return ()

runProgram :: Pipeline a -> Task a
runProgram = foldFree executor . foldFree interpreter

interpreter :: PipelineF a -> Free (Sum Logger.LogF Task) a
interpreter op =
  toLeft (LogI.interpreter op) *> toRight (lift $ PipelineI.interpreter op)

executor :: Sum Logger.LogF Task a -> Task a
executor (InL l@(Logger.Log _ _ next)) = lift $ Logger.executor l >> return next
executor (InR io) = io
