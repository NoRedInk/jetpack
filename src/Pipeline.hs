{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Pipeline
  ( Args(..)
  , Pipeline
  , PipelineF(..)
  , Runnable
  , RunnableF(..)
  , readCliArgs
  , readConfig
  , noArgs
  , compile
  , printStr
  ) where

import Config (Config)
import Control.Monad.Free (Free, liftF)
import Data.Text as T
import System.FilePath ()

-- TODO move this to args parser
data Args = Args
  { dry :: Bool
  , verbose :: Bool
  , configPath :: Maybe FilePath
  }

noArgs :: Args
noArgs = Args False False Nothing

data PipelineF a
  = ReadCliArgs (Args -> a)
  | ReadConfig (Maybe FilePath)
               (Config -> a)
  | Compile ([T.Text] -> a)
  deriving (Functor)

type Pipeline = Free PipelineF

data RunnableF a =
  Run (IO ())
      a
  deriving (Functor)

type Runnable = Free RunnableF

readCliArgs :: Pipeline Args
readCliArgs = liftF $ ReadCliArgs id

readConfig :: Maybe FilePath -> Pipeline Config
readConfig maybePath = liftF $ ReadConfig maybePath id

compile :: Pipeline [T.Text]
compile = liftF $ Compile id

printStr :: IO () -> Runnable ()
printStr io = liftF $ Run io ()
