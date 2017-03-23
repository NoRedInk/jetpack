{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Parser for `import`s in elm.
* It returns a list of `Elm String`
* It ignores `import`s in commments.


### imports for doctests
   >>> import Parser.Ast
   >>> import qualified Data.Text as T
   >>> :set -XOverloadedStrings
-}
module Parser.Import where

import Data.Maybe as M
import qualified Data.Text as T
import Parser.Ast as Ast
import Parser.Comment as Comment
import Text.Parsec


{-| returns all imports of a file
    >>> :{
    imports $
      T.unlines
      [ "module Main exposing (..)"
      , "import Html exposing (Html, text)"
      , "import Maybe.Extra"
      , "-- import Dict.Extra"
      , "foo = \"import Dict.Extra\""
      , ""
      , "main : Html a"
      , "main ="
      , "  text \"Hello, World!\""
      ]
    :}
    [(Import "Html"),(Import "Maybe.Extra")]
-}
imports :: T.Text -> [Ast.Require]
imports = M.mapMaybe import' . T.lines . Comment.eatElmComments

{-| Parses a `import`s.
   >>> import' "import Page.Foo.Bar exposing (view)"
   Just (Import "Page.Foo.Bar")

   >>> import' "require Page.Foo.Bar exposing (view)"
   Nothing
-}
import' :: T.Text -> Maybe Ast.Require
import' content =
  case extractImport content of
    Right mod -> Just $ Ast.Import $ T.pack mod
    Left _    -> Nothing

{-| running the parser
   >>> extractImport "import Page.Foo.Bar exposing (view)"
   Right "Page.Foo.Bar"

   >>> extractImport "import Page"
   Right "Page"

   >>> extractImport " import Page"
   Left "Error" (line 1, column 1):
   unexpected " "
   expecting "import"

   >>> extractImport "importPage"
   Left "Error" (line 1, column 7):
   unexpected "P"
   expecting space
-}
extractImport :: T.Text -> Either ParseError String
extractImport str = parse importParser "Error" str

importParser :: Parsec T.Text u String
importParser = do
  _ <- string "import"
  _ <- many1 space
  many1 $ oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_."
