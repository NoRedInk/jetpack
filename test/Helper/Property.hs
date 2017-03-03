{-# LANGUAGE OverloadedStrings #-}

module Helper.Property where

import Data.Text as T
import Test.Tasty.QuickCheck

newtype AlphaNum =
  AlphaNum T.Text
  deriving (Show)

instance (Arbitrary AlphaNum) where
  arbitrary = fmap (AlphaNum . T.pack) letterOrDigit

alphaFreqList :: [(Int, Gen Char)]
alphaFreqList =
  [(26, choose ('a', 'z')), (26, choose ('A', 'Z')), (1, elements ['_'])]

digitFreqList :: [(Int, Gen Char)]
digitFreqList = [(10, choose ('0', '9'))]

letter :: Gen Char
letter = frequency alphaFreqList

letterOrDigit :: Gen String
letterOrDigit = listOf1 $ frequency $ alphaFreqList ++ digitFreqList
