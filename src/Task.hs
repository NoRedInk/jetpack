{-# LANGUAGE NamedFieldPuns #-}

{-| Task is an enhanced IO, it holds state and allows to stop all computation by throwing an error.
-}
module Task
  ( Task
  , ExceptIO
  , toTask
  , runTask
  , getArgs
  , getConfig
  ) where

import Control.Monad.Except
import Control.Monad.State
import Env
import Error

type ExceptIO = ExceptT [Error] IO
type Task = StateT Env ExceptIO

toTask :: IO a -> Task a
toTask = lift . lift


runTask :: Monad m => StateT Env (ExceptT e m) a -> m (Either e a)
runTask t = runExceptT $ evalStateT t $ Env
  { args = undefined
  , config = undefined
  , progressBar = Nothing
  }


{-| Get config from state.
-}
getConfig :: Task Config
getConfig = do
  Env { config } <- get
  return config

{-| Get args from state.
-}
getArgs :: Task Args
getArgs = do
  Env { args } <- get
  return args
