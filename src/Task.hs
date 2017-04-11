{-# LANGUAGE NamedFieldPuns #-}
module Task
  (
  Task
  , ExceptIO
  , toTask
  , runTask
  , getConfig
  ) where

import Control.Monad.Except
import Control.Monad.State
import Env
import Error
import qualified System.Console.AsciiProgress as Progress

type ExceptIO = ExceptT [Error] IO
type Task = StateT Env ExceptIO

toTask :: IO a -> Task a
toTask = lift . lift

runTask :: Monad m => StateT Env (ExceptT e m) a -> m (Either e a)
runTask t = runExceptT $ evalStateT t $ Env { progressBar = Nothing }


{-| Get config from state.
-}
getConfig :: Task Config
getConfig = do
  Env { config } <- get
  return config
