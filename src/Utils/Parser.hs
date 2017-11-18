{-# LANGUAGE OverloadedStrings #-}

module Utils.Parser
  ( stringContent
  , betweenParens
  , eatTill
  ) where

import qualified Data.Text as T
import qualified Text.Parsec as P

{-| imports for doctests
    >>> :set -XOverloadedStrings
    >>> import qualified Text.Parsec as P
    >>> import qualified Data.Text as T
-}
{-| parses text between quotes or double qoutes
    >>> P.parse stringContent "invalid" "\"hello\""
    Right "hello"

    >>> P.parse stringContent "invalid" "'hello'"
    Right "hello"

    >>> P.parse stringContent "invalid" "''"
    Left "invalid" (line 1, column 2):
    unexpected "'"

    >>> P.parse stringContent "invalid" "(foo)"
    Left "invalid" (line 1, column 1):
    unexpected "("
    expecting "'" or "\""
-}
stringContent :: P.Parsec T.Text u String
stringContent = P.choice [quotes manyNotQuotes, doubleQuotes manyNotQuotes]

{-| parses text between parens
    >>> P.parse (betweenParens stringContent) "invalid" "('hello')"
    Right "hello"

    >>> P.parse (betweenParens stringContent) "invalid" "\"foo\""
    Left "invalid" (line 1, column 1):
    unexpected "\""
    expecting "("
-}
betweenParens :: P.Parsec T.Text u String -> P.Parsec T.Text u String
betweenParens = P.between openAndSpaces spacesAndClose
  where openAndSpaces = (P.char '(') *> (P.skipMany P.space)
        spacesAndClose = (P.skipMany P.space) *> (P.char ')')


manyNotQuotes :: P.Parsec T.Text u String
manyNotQuotes = P.many1 $ P.noneOf "'\""

between :: P.Parsec T.Text u Char
        -> P.Parsec T.Text u String
        -> P.Parsec T.Text u String
between c = P.between c c

quotes :: P.Parsec T.Text u String -> P.Parsec T.Text u String
quotes = between $ P.char '\''

doubleQuotes :: P.Parsec T.Text u String -> P.Parsec T.Text u String
doubleQuotes = between $ P.char '"'

{-| parses text between parens
    >>> P.parse (eatTill $ P.string "end") "invalid" "beginn foo end"
    Right "beginn foo "
-}
eatTill :: P.Parsec T.Text u String -> P.Parsec T.Text u String
eatTill p = P.manyTill P.anyChar (P.lookAhead $ P.try p)
