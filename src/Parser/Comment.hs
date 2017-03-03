{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Parser.Comment
  ( eatComments
  ) where

import Data.Functor (void)
import qualified Data.List as L
import qualified Data.Text as T
import Text.Parsec

eatComments :: T.Text -> T.Text
eatComments str =
  case (parse parser "Error" str) of
    Right parsed -> parsed
    Left _ -> str
  where
    parser = do
      eatCommentsParser (try blockCommentParser <|> try commentParser)

eatCommentsParser :: Parsec T.Text st () -> Parsec T.Text st T.Text
eatCommentsParser parser = do
  optional parser
  xs <- sepBy (notCommentParser parser) parser
  optional parser
  return $ T.pack $ L.intercalate "" xs

blockCommentParser :: Parsec T.Text st ()
blockCommentParser = string "/*" >> manyTill anyChar (string "*/") >> return ()

commentParser :: Parsec T.Text st ()
commentParser =
  string "//" >> manyTill anyChar (void newline <|> eof) >> return ()

notCommentParser :: Parsec T.Text st () -> Parsec T.Text st String
notCommentParser parser = manyTill anyChar (lookAhead (parser <|> eof))
