{-# LANGUAGE DeriveFunctor             #-}

{-| This is our grammar to build jetpack.
-}
module Algebra.Setup where

import CliArguments (Args)
import Config (Config)
import Control.Monad.Free (Free, liftF)
import System.FilePath ()
import ToolPaths

data SetupF next
  = ReadCliArgs (Args -> next)
  | ReadConfig (Maybe FilePath) (Config -> next)
  | Setup (ToolPaths -> next)
  | ClearLog next
  deriving (Functor)

type Setup = Free SetupF

-- Helper functions to create a `Free SetupF`
--

readCliArgs :: Setup Args
readCliArgs = liftF $ ReadCliArgs id

readConfig :: Maybe FilePath -> Setup Config
readConfig maybePath = liftF $ ReadConfig maybePath id

setup :: Setup ToolPaths
setup  = liftF $ Setup id

clearLog :: Setup ()
clearLog = liftF $ ClearLog ()
