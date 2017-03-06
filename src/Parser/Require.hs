{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

{-| Parser for `require` in js and coffeescript.
* It returns a list of `Require (Coffee|Js|Elm|Sass) fileName`
* It ignores `require` in commments.


### imports for doctests
   >>> import qualified Data.Text as T
   >>> :set -XOverloadedStrings
-}
module Parser.Require
  ( requires
  , require
  , getFileType
  , Require(..)
  , SourceType(..)
  ) where

import qualified Data.Maybe as M
import qualified Data.Text as T
import Parser.Comment as Comment
import System.FilePath ((<.>), splitExtension)
import Text.Parsec
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
  deriving (Show, Eq)

{-| returns all requires of a file
    >>> :{
    requires $
      T.unlines
      [ "var _ = require('lodash')"
      , "var Main = require('Foo.Bar.Main.elm')"
      , ""
      , "// var Main = require('Foo.Bar.Main.elm')"
      , "console.log('42'); /*"
      , "var Main = require('Foo.Bar.Main.elm')"
      , "*/"
      , "Main.embed(document.getElementById('host'), {})"
      , "function require(foo) {"
      , "  console.log('local require')"
      , "}"
      ]
    :}
    [(Require "lodash" Js),(Require "Foo.Bar.Main.elm" Elm)]
-}
requires :: T.Text -> [Require]
requires = M.mapMaybe require . T.lines . Comment.eatComments

{-| Parses a require statement and returns the filename and the type base on the extensions.

    >>> require "require('lodash')"
    Just (Require "lodash" Js)

    >>> require "require('Main.elm')"
    Just (Require "Main.elm" Elm)

    >>> require "require('Main.elm';"
    Nothing
-}
require :: T.Text -> Maybe Require
require content =
  case extractRequire content of
    Right (path, ext) -> Just $ Require (getFileType ext) $ path <.> ext
    Left _ -> Nothing

{-| Converts a file extension into a union type.

    >>> getFileType ".coffee"
    Coffee

Default for an empty extension or something unknown is js.
This is because you might importe something like `require('MyModule.Foo')`
    >>> getFileType ""
    Js
-}
getFileType :: String -> SourceType
getFileType ".coffee" = Coffee
getFileType ".elm" = Elm
getFileType ".sass" = Sass
getFileType ".js" = Js
getFileType _ = Js

{-| running the parser
-}
extractRequire :: T.Text -> Either ParseError (FilePath, String)
extractRequire str = parse requireParser "Error" str

requireParser :: Parsec T.Text u (FilePath, String)
requireParser = do
  _ <- ignoreTillRequire
  _ <- requireKeyword
  _ <- spaces
  content <- choice [UP.betweenParens UP.stringContent, UP.stringContent]
  return $ splitExtension content

ignoreTillRequire :: Parsec T.Text u String
ignoreTillRequire = manyTill anyChar (lookAhead $ try requireKeyword)

requireKeyword :: Parsec T.Text u String
requireKeyword = string "require"
