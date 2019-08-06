module Parser.Ast
  ( Require (..)
  , SourceType (..)
  )
where

import Protolude hiding (show)
import Data.Aeson as Aeson
import GHC.Generics (Generic)
import System.FilePath ()
import Text.Show (Show, show)

data Require
  = Require
      SourceType
      FilePath
  deriving (Eq)

instance Show Require where

  show (Require t n) = "(Require " ++ show n ++ " " ++ show t ++ ")"

data SourceType
  = Coffee
  | Js
  | Elm
  deriving (Show, Eq, Generic)

instance FromJSON SourceType

instance ToJSON SourceType
