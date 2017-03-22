{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Parser for line and block comments in js or coffeescript.


### imports for doctests
   >>> :set -XOverloadedStrings
   >>> import qualified Data.Text as T
-}
module Parser.Comment
  ( eatJsComments
  , eatCoffeeComments
  , eatElmComments
  ) where

import Data.Functor (void)
import qualified Data.List as L
import qualified Data.Text as T
import Text.Parsec

{-| Removes block and line comments from text.
    >>> :{
    eatElmComments $
      T.unlines
        [ "import Page.Foo.Bar"
        , "-- import Maybe.Extra"
        , "type Msg = NoOp"
        , "{- some comment -}"
        , "foo = 42"
        ]
    :}
    "import Page.Foo.Bar\ntype Msg = NoOp\n\nfoo = 42\n"
-}
eatElmComments :: T.Text -> T.Text
eatElmComments = eatComments elmBlockCommentParser elmLineCommentParser

{-| Removes block and line comments from text.
    >>> :{
    eatJsComments $
      T.unlines
        [ "var x = require('x.js');"
        , "// var x = require('x.js');"
        , "x.foo(); /* comment */"
        ]
    :}
    "var x = require('x.js');\nx.foo(); \n"
-}
eatJsComments :: T.Text -> T.Text
eatJsComments = eatComments jsBlockCommentParser jsLineCommentParser

{-| Removes block and line comments from text.
    >>> :{
    eatCoffeeComments $
      T.unlines
        [ "var x = require('x.js');"
        , "# var x = require('x.js');"
        , "x.foo();"
        , "###"
        , "ignore"
        , "###"
        ]
    :}
    "var x = require('x.js');\nx.foo();\n\n"
-}
eatCoffeeComments :: T.Text -> T.Text
eatCoffeeComments = eatComments coffeeBlockCommentParser coffeeLineCommentParser

eatComments :: Parsec T.Text () () -> Parsec T.Text () () -> T.Text -> T.Text
eatComments blockParser lineParser str =
  case (parse parser "Error" str) of
    Right parsed -> parsed
    Left _       -> str
  where
    parser = eatCommentsParser (try blockParser <|> try lineParser)

eatCommentsParser :: Parsec T.Text st () -> Parsec T.Text st T.Text
eatCommentsParser parser = do
  optional parser
  xs <- sepBy (notCommentParser parser) parser
  optional parser
  return $ T.pack $ L.intercalate "" xs

elmBlockCommentParser :: Parsec T.Text st ()
elmBlockCommentParser =
  string "{-" >> manyTill anyChar (try $ string "-}") >> return ()

elmLineCommentParser :: Parsec T.Text st ()
elmLineCommentParser =
  string "--" >> manyTill anyChar (void newline <|> eof) >> return ()


jsBlockCommentParser :: Parsec T.Text st ()
jsBlockCommentParser =
  string "/*" >> manyTill anyChar (try $ string "*/") >> return ()

jsLineCommentParser :: Parsec T.Text st ()
jsLineCommentParser =
  string "//" >> manyTill anyChar (void newline <|> eof) >> return ()

coffeeBlockCommentParser :: Parsec T.Text st ()
coffeeBlockCommentParser =
  string "###" >> manyTill anyChar (string "###") >> return ()

coffeeLineCommentParser :: Parsec T.Text st ()
coffeeLineCommentParser =
  string "#" >> manyTill anyChar (void newline <|> eof) >> return ()

notCommentParser :: Parsec T.Text st () -> Parsec T.Text st String
notCommentParser parser = manyTill anyChar (lookAhead (parser <|> eof))
