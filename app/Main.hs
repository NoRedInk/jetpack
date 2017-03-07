{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Free (Free, foldFree, liftF)
import Data.String (IsString)
import Data.Text as T
import Lib (run)

newtype Args =
  Args T.Text
  deriving (IsString)

newtype Config =
  Config T.Text
  deriving (IsString)

newtype Module =
  Module T.Text
  deriving (IsString)

data PipelineF a
  = ReadCliArgs (Args -> a)
  | ReadConfig Args
               (Config -> a)
  | Compile ([Module] -> a)
  deriving (Functor)

type Pipeline = Free PipelineF

readCliArgs :: Pipeline Args
readCliArgs = liftF $ ReadCliArgs id

readConfig :: Args -> Pipeline Config
readConfig path = liftF $ ReadConfig path id

compile :: Pipeline [Module]
compile = liftF $ Compile id

program :: Pipeline ()
program = do
  args <- readCliArgs
  config <- readConfig args
  modules <- compile
  return ()

stepPrintI :: PipelineF a -> IO a
stepPrintI (ReadCliArgs next) = putStrLn "readCliArgs" >> return (next "")
stepPrintI (ReadConfig args next) = putStrLn "readConfig" >> return (next "")
stepPrintI (Compile next) = putStrLn "compile" >> return (next [])

main :: IO ()
main = do
  foldFree stepPrintI program
  -- 1. read config
  -- 2. find dependencies of entry points (magic requires (.sass/.elm)?) (how many levels? we should probably find elm deps as wel)
  -- 3. create binary with tree
  -- 3. build cache (not mvp)
  --    a. create binary with file modified date and hash of content
  -- 4. compile
  --    a. check cache if we need to build this. first date and the hash
  -- 5. recurisvely replace requires
  -- 6. compress (if prod)
