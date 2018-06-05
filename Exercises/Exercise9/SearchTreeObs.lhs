> module SearchTreeObs where
>
> import Observer
> import SearchTree
> import Prelude hiding (lookup)

In the implementation of Observers from the lecture we defined generic observers o0 and o2.

(1) Define similar generic observers o1, o3, and o4 for observing constructors of the corresponding arities.

The additional convenience functions are defined in `Observer.hs`.

(2) Modify the implementation of Tree to a data type for SearchTrees, such that you use the type class Ord for the keys.

The definition for `SearchTree` can be find in `SearchTree.hs`.

Use the observer library to observe the behaviour of the functions of your search tree.
