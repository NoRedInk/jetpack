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
import Errors (Error(..))
import System.FilePath ((<.>), splitExtension)
import qualified Text.Parsec as P
import qualified Utils.Parser as UP

data Require = Require
  { fileType :: SourceType
  , fileName :: FilePath
  } deriving (Show, Eq)

data SourceType
  = Coffee
  | Js
  | Elm
  | Sass
  | Unsupported
  deriving (Show, Eq)

{-| Parses a require statement and returns the filename and the type base on the extensions.
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

requireParser :: P.Parsec T.Text u0 (FilePath, String)
requireParser = do
  _ <- P.manyTill P.anyChar (P.lookAhead $ P.try requireKeyword)
  _ <- requireKeyword
  _ <- P.oneOf " ("
  _ <- P.oneOf "'\""
  content <- P.manyTill P.anyChar (P.oneOf "'\"")
  _ <- P.oneOf " )\n"
  return $ splitExtension content
  where
    requireKeyword = P.string "require"

getFileType :: String -> SourceType
getFileType ".coffee" = Coffee
getFileType ".elm" = Elm
getFileType ".sass" = Sass
getFileType ".js" = Js
getFileType _ = Unsupported
