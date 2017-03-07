{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Error
  ( Error(..)
  , description
  ) where

data Error
  = FileNotFound String
  | JsonInvalid String

description :: Error -> String
description (FileNotFound file) = "Couldn't find file: " ++ file
description (JsonInvalid file) = "Invalid json file: " ++ file
