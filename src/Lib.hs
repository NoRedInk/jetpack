{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( run
  ) where

import qualified Config
import Control.Monad.Free (Free, foldFree, liftF, hoistFree)
import Data.Functor.Sum (Sum(..))
import Data.Text as T
import qualified Logger
import Pipeline

toLeft
  :: (Functor f, Functor g)
  => Free f a -> Free (Sum f g) a
toLeft = hoistFree InL

toRight
  :: (Functor f, Functor g)
  => Free g a -> Free (Sum f g) a
toRight = hoistFree InR

program :: Pipeline ()
program = do
  args <- readCliArgs
  config <- readConfig (configPath args)
  _ <- compile
  return ()

stepRunnableI :: PipelineF a -> Runnable a
stepRunnableI (ReadCliArgs next) =
  printStr (putStrLn "readCliArgs") >> return (next noArgs)
stepRunnableI (ReadConfig args next) =
  printStr (putStrLn "readConfig") >> return (next Config.defaultConfig)
stepRunnableI (Compile next) = printStr (putStrLn "compile") >> return (next [])

logI :: PipelineF a -> Logger.Log ()
logI (ReadCliArgs next) = Logger.info "readCliArgs"
logI (ReadConfig maybePath next) = Logger.info "readConfig"
logI (Compile next) = Logger.info "compile"

interpreter :: PipelineF a -> Free (Sum Logger.LogF RunnableF) a
interpreter op = toLeft (logI op) *> toRight (stepRunnableI op)

executor :: Sum Logger.LogF RunnableF a -> IO a
executor (InL log@(Logger.Log _ _ next)) = Logger.executor log >> return next
executor (InR (Run io next)) = io >> return next

run :: IO ()
run = do
  foldFree executor $ foldFree interpreter program
