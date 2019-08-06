module Utils.Tree
  ( searchNode
  , foldTree_
  , roots
  , nodesWithChildren
  )
where

import Protolude
import Data.Tree

{-| The first child that satisfies a predicate.
    >>> let myTree = Node 1 [ Node 11 [], Node 12 [ Node 21 [ Node 31 [] ] , Node 22 [] ] ]
    >>> searchNode ((==) 22 . rootLabel) myTree
    Just (Node {rootLabel = 22, subForest = []})

    >>> searchNode ((==) 1 . rootLabel) myTree == Just myTree
    True

    >>> searchNode ((==) 23 . rootLabel) myTree
    Nothing

    >>> searchNode ((==) 12 . rootLabel) myTree
    Just (Node {rootLabel = 12, subForest = [Node {rootLabel = 21, subForest = [Node {rootLabel = 31, subForest = []}]},Node {rootLabel = 22, subForest = []}]})
-}
searchNode :: (Tree a -> Bool) -> Tree a -> Maybe (Tree a)
searchNode p = find . pure
  where
    find (x : _)
      | p x = Just x
    find (x : xs) = find (xs ++ subForest x)
    find [] = Nothing

{-| Fold over a tree.
    >>> let myTree = Node 1 [ Node 11 [], Node 12 [ Node 21 [ Node 31 [] ] , Node 22 [] ] ]
    >>> foldTree_ (\a as -> show a : fmap show as) myTree
    ["1","11","12","11","12","21","22","21","31","31","22"]
-}
foldTree_ :: Monoid m => (a -> [a] -> m) -> Tree a -> m
foldTree_ f (Node x ts) = mconcat $ f x (roots ts) : fmap (foldTree_ f) ts

{-| Get all nodes and the children of each node.
    >>> let myTree = Node 1 [ Node 11 [], Node 12 [ Node 21 [ Node 31 [] ] , Node 22 [] ] ]
    >>> nodesWithChildren myTree
    [(1,[11,12]),(11,[]),(12,[21,22]),(21,[31]),(31,[]),(22,[])]
-}
nodesWithChildren :: Tree a -> [(a, [a])]
nodesWithChildren = foldTree_ (\a as -> [(a, as)])

{-| Get all rootLabels of a Forest.
    >>> let myForest = [ Node 11 [], Node 12 [ Node 21 [ Node 31 [] ] , Node 22 [] ], Node 13 [] ]
    >>> roots myForest
    [11,12,13]
-}
roots :: Forest a -> [a]
roots = fmap rootLabel
