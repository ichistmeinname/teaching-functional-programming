> module FullTree where

Define a data type `FullTree` a for full binary tree (each node has either two or zero children)
 with labels in its leaves. The property of being a full binary tree is supposed to be encoded
in the type. That is, it should not be possible to construct a value of type FullTree a, which
violates the above property.

Define a function `fullTree :: Int -> FullTree Int` that create a full binary tree with a given
height $n$, where the leaves are enumerated from 1 to $2^n$.

Hint: First, define a function `fork :: FullTree a -> FullTree a -> FullTree a` that combines two
tree of the same size and use this function to define fullTree.

Similar to ArrayLists, FullTree has two constructors, one for leaves and one for branching.
However, the branch constructor `Fork` uses a nested data type to balance the heights of both brances.
        
> data FullTree a = Leaf a | Fork (FullTree (a,a)) deriving Show

Based on the data type definition we ca define the following values.

> fullTree1 :: FullTree Int
> fullTree1 = Leaf 1
> 
> fullTree2 :: FullTree Int
> fullTree2 = Fork (Leaf (2,3))
> 
> fullTree3 :: FullTree Int
> fullTree3 = Fork (Fork (Leaf ((4,5),(6,7))))

That is, the constructor `Fork` gives insights about the height of a tree.

We define a partial function `fork` that combines two trees of the same height!

> fork :: FullTree a -> FullTree a -> FullTree a
> fork (Leaf x)  (Leaf y)  = Fork $ Leaf (x,y)
> fork (Fork t1) (Fork t2) = Fork $ fork t1 t2

Yes, it's really that easy: we just construct a new `Fork` and combine the values using pairs.

In order to define a full tree labeled with numbers in ascending order, we need a helper function
that keeps track of the current depth level and the smallest label.

> fullTree :: Int -> FullTree Int
> fullTree n = mkTree n 1
>  where

Using these additional parameters we can compute the corresponding label for the leaves.

>   mkTree :: Int -> Int -> FullTree Int
>   mkTree depth offset
>     | depth == 0 = Leaf offset
>     | otherwise  = fork (mkTree d' offset) (mkTree d' (offset + 2 ^ d'))
>    where d' = depth - 1

Finally, let's take a look at some tests.

    λ> mapM_ (print . fullTree) [0..3]
    Leaf 1
    Fork (Leaf (1,2))
    Fork (Fork (Leaf ((1,2),(3,4))))
    Fork (Fork (Fork (Leaf (((1,2),(3,4)),((5,6),(7,8))))))
