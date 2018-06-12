> module FullTree where

Define a data type `FullTree` a for full binary tree (each node has either two or zero children)
 with labels in its leaves. The property of being a full binary tree is supposed to be encoded
in the type. That is, it should not be possible to construct a value of type FullTree a, which
violates the above property.

Define a function `fullTree :: Int -> FullTree Int` that create a full binary tree with a given
height $n$, where the leaves are enumerated from 1 to $2^n$.

Hint: First, define a function `fork :: FullTree a -> FullTree a -> FullTree a` that combines two
tree of the same size and use this function to define fullTree.

> data FullTree a -- to be defined
>
> fullTree :: Int -> FullTree Int
> fullTree = undefined
>
> fork :: FullTree a -> FullTree a -> FullTree a
> fork = undefined
