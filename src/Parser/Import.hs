{-| Parser for `import`s in elm.
* It returns a list of `Elm String`
* It ignores `import`s in commments.


### imports for doctests
   >>> import Parser.Ast
   >>> import qualified Data.Text as T
-}
module Parser.Import where

import qualified Data.Text as T
import Parser.Ast as Ast
import Text.Parsec
import qualified Utils.Parser as UP

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
    Left _ -> Nothing

{-| running the parser
   >>> extractImport "import Page.Foo.Bar exposing (view)"
   Right "Page.Foo.Bar"

   >>> extractImport "import Page"
   Right "Page"

   >>> extractImport "importPage"
   Left "Error" (line 1, column 7):
   unexpected "P"
   expecting space
-}
extractImport :: T.Text -> Either ParseError String
extractImport str = parse importParser "Error" str

importParser :: Parsec T.Text u String
importParser = do
  _ <- UP.eatTill importKeyword
  _ <- importKeyword
  _ <- many1 space
  many1 $ oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_."

importKeyword :: Parsec T.Text u String
importKeyword = string "import"
