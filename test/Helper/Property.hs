{-# LANGUAGE OverloadedStrings #-}

{-| Helpers for property based tests.
-}
module Helper.Property where

import Data.Text as T
import Test.Tasty.QuickCheck as QC

newtype AlphaNum =
  AlphaNum T.Text
  deriving (Show)

instance (QC.Arbitrary AlphaNum) where
  arbitrary = fmap (AlphaNum . T.pack) letterOrDigit

newtype CodeNoComments =
  CodeNoComments T.Text
  deriving (Show)

instance (QC.Arbitrary CodeNoComments) where
  arbitrary = fmap (CodeNoComments . T.pack) code

alphaFreqList :: [(Int, QC.Gen Char)]
alphaFreqList =
  [ (26, QC.choose ('a', 'z'))
  , (26, QC.choose ('A', 'Z'))
  , (1, QC.elements ['_'])
  ]

digitFreqList :: [(Int, QC.Gen Char)]
digitFreqList = [(10, QC.choose ('0', '9'))]

symbolsFreqList :: [(Int, QC.Gen Char)]
symbolsFreqList = [(5, QC.elements ['+', ';', '*', '%'])]

letter :: QC.Gen Char
letter = QC.frequency alphaFreqList

letterOrDigit :: QC.Gen String
letterOrDigit = listOf1 $ QC.frequency $ alphaFreqList ++ digitFreqList

code :: QC.Gen String
code =
  listOf1 $ QC.frequency $ alphaFreqList ++ digitFreqList ++ symbolsFreqList
