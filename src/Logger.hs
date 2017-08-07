{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( clearLog, appendLog
  ) where


import Config
import Data.Text as T
import System.FilePath ((</>))
import Task

appendLog :: T.Text -> T.Text -> Task ()
appendLog fileName msg = do
  Config {log_directory} <- Task.getConfig
  toTask $ appendFile (log_directory </> (T.unpack fileName)) $ T.unpack msg

clearLog :: T.Text -> Task ()
clearLog fileName = do
  Config {log_directory} <- Task.getConfig
  toTask $ writeFile (log_directory </> (T.unpack fileName)) ""
