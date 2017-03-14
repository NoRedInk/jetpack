{-# LANGUAGE DeriveGeneric #-}
-- TODO move this to a better place. This is not an AST
module Parser.Ast
  ( Require(..)
  , SourceType(..)
  ) where

import Data.Aeson as Aeson
import GHC.Generics (Generic)
import System.FilePath ()


data Require = Require
  { fileType :: SourceType
  , fileName :: FilePath
  } deriving (Eq)

instance Show Require where
  show (Require t n) = "(Require " ++ show n ++ " " ++ show t ++ ")"

data SourceType
  = Coffee
  | Js
  | Elm
  | Sass
  deriving (Show, Eq, Generic)

instance FromJSON SourceType
instance ToJSON SourceType
