{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.Parser where

import qualified Data.Text as T
import qualified Text.Parsec as P

between :: P.Parsec T.Text u Char -> P.Parsec T.Text u String-> P.Parsec T.Text u String
between c = P.between c c

quotes :: P.Parsec T.Text u String -> P.Parsec T.Text u String
quotes = between $ P.char '\''

doubleQuotes :: P.Parsec T.Text u String -> P.Parsec T.Text u String
doubleQuotes = between $ P.char '"'
