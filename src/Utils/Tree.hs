{-# OPTIONS_GHC -Wall #-}
module Utils.Tree (findDescendant) where

import Data.Tree

{-| The first child that satisfies a predicate.
    >>> let myTree = Node 1 [ Node 11 [], Node 12 [ Node 21 [ Node 31 [] ] , Node 22 [] ] ]
    >>> findDescendant ((==) 22 . rootLabel) myTree
    Just (Node {rootLabel = 22, subForest = []})

    >>> findDescendant ((==) 1 . rootLabel) myTree == Just myTree
    True

    >>> findDescendant ((==) 23 . rootLabel) myTree
    Nothing

    >>> findDescendant ((==) 12 . rootLabel) myTree
    Just (Node {rootLabel = 12, subForest = [Node {rootLabel = 21, subForest = [Node {rootLabel = 31, subForest = []}]},Node {rootLabel = 22, subForest = []}]})
-}
findDescendant :: (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
findDescendant p = find . pure
  where find (x:_)  | p x  = Just x
        find (x:xs) = find (xs ++ subForest x)
        find []     = Nothing
