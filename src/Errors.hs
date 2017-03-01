{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Errors
  ( Error(..)
  , description
  ) where

data Error
  = FileNotFound String
  | JsonInvalid String

description :: Error -> String
description (FileNotFound file) = "Couldn't find file: " ++ file
description (JsonInvalid file) = "Invalid json file: " ++ file
