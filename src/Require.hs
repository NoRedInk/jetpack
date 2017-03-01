{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Require
  ( require
  , fromPathAndExt
  , Require(..)
  , SourceType(..)
  ) where

import qualified Data.Text as T

-- TODO use this import Errors (Error(..))
import System.FilePath ((<.>), splitExtension)
import qualified Text.Parsec as P
import qualified Utils.Parser as UP

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
  | Unsupported
  deriving (Show, Eq)

{-| imports for doctests
   >>> :set -XOverloadedStrings
-}
{-| Parses a require statement and returns the filename and the type base on the extensions.

    >>> require "require('lodash')"
    Right (Require "lodash" Js)

    >>> require "require('Main.elm')"
    Right (Require "Main" Elm)

    >>> require "require('Main.elm';"
    Left "\"Error\" (line 1, column 19):\nunexpected \";\"\nexpecting \")\""
-}
require :: T.Text -> Either T.Text Require
require content =
  case extractRequire content of
    Right (path, ext) -> Right $ fromPathAndExt path ext
    Left msg -> Left $ T.pack $ show msg

fromPathAndExt :: FilePath -> String -> Require
fromPathAndExt path ext =
  case getFileType ext of
    Unsupported -> Require Js $ path <.> ext
    _ -> Require (getFileType ext) path

{-| running the parser
-}
extractRequire :: T.Text -> Either P.ParseError (FilePath, String)
extractRequire str = P.parse requireParser "Error" str

requireParser :: P.Parsec T.Text u (FilePath, String)
requireParser = do
  _ <- ignoreTillRequire
  _ <- requireKeyword
  _ <- P.spaces
  content <- P.choice [requireBetweenParens, coffeeRequire]
  return $ splitExtension content

requireBetweenParens :: P.Parsec T.Text u String
requireBetweenParens = UP.betweenParens UP.stringContent

coffeeRequire :: P.Parsec T.Text u String
coffeeRequire = P.spaces *> UP.stringContent

requireKeyword :: P.Parsec T.Text u String
requireKeyword = P.string "require"

ignoreTillRequire :: P.Parsec T.Text u String
ignoreTillRequire = P.manyTill P.anyChar (P.lookAhead $ P.try requireKeyword)

getFileType :: String -> SourceType
getFileType ".coffee" = Coffee
getFileType ".elm" = Elm
getFileType ".sass" = Sass
getFileType ".js" = Js
getFileType _ = Unsupported
