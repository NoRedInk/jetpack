{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Require
  ( require
  , Require(..)
  , SourceType(..)
  ) where

import qualified Data.Text as T
import Errors (Error(..))
import qualified Text.Parsec as P

data Require = Require
  { fileName :: T.Text
  , fileType :: SourceType
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
    Right (name, ext) ->
      case getFileType ext of
        Unsupported -> Right $ Require (T.concat [name, ext]) Js
        _ -> Right $ Require (T.dropEnd 1 name) $ getFileType ext
    Left msg -> Left $ T.pack $ show msg

{-| running the parser
-}
extractRequire :: T.Text -> Either P.ParseError (T.Text, T.Text)
extractRequire str = P.parse requireParser "Error" str

requireParser :: P.Parsec T.Text u0 (T.Text, T.Text)
requireParser = do
  _ <- P.manyTill P.anyChar (P.lookAhead $ P.try requireKeyword)
  _ <- requireKeyword
  _ <- P.oneOf " ("
  _ <- P.oneOf "'\""
  content <- P.manyTill P.anyChar (P.oneOf "'\"")
  _ <- P.oneOf " )\n"
  return $ T.breakOnEnd "." $ T.pack content
  where
    requireKeyword = P.string "require"

getFileType :: T.Text -> SourceType
getFileType "coffee" = Coffee
getFileType "elm" = Elm
getFileType "sass" = Sass
getFileType "js" = Js
getFileType _ = Unsupported
