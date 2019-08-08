module Dependencies
  ( Dependencies
  , Dependency (..)
  , DependencyTree
  )
where

import Data.Aeson as Aeson
import Data.Time.Clock
import qualified Data.Tree as Tree
import GHC.Generics (Generic)
import qualified Parser.Ast as Ast
import System.FilePath ()

data Dependency
  = Dependency
      { fileType :: Ast.SourceType
      , requiredAs :: FilePath
      , filePath :: FilePath
      , lastModificationTime :: Maybe UTCTime
      }
  deriving (Eq, Generic)

instance FromJSON Dependency

instance ToJSON Dependency

instance Show Dependency where

  show (Dependency t r p l) =
    "(Dependency: " ++
      show r ++
      " " ++
      show t ++
      " <" ++
      show p ++
      "> " ++
      show l ++
      ")"

type DependencyTree = Tree.Tree Dependency

type Dependencies = [DependencyTree]
