{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Dependencies
  ( Dependencies
  , Dependency(..)
  , DependencyTree
  ) where

import Data.Aeson as Aeson
import Data.Time.Clock
import qualified Data.Tree as Tree
import GHC.Generics (Generic)
import qualified Parser.Ast as Ast
import System.FilePath ()

data Dependency = Dependency
  { fileType             :: Ast.SourceType
  , requiredAs           :: FilePath
  , filePath             :: FilePath
  , lastModificationTime :: Maybe UTCTime
  , isMain               :: Bool
  } deriving (Eq, Generic, Show)

instance FromJSON Dependency
instance ToJSON Dependency

type DependencyTree = Tree.Tree Dependency

type Dependencies = [DependencyTree]
