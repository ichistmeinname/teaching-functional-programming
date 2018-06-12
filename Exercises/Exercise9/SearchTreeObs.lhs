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

First, we define an instance of `Observe`.
Note that our newly defined smart constructors come in handy here.

> instance (Observe k, Observe v) => Observe (Tree k v) where
>     obs Empty          = o0 Empty "Empty"
>     obs (Node l k v r) = o4 Node  "Node" l k v r

Find examples, which visualise, which parts of the tree are computed if a new entry is added or a key is lokked up.

Let's define an example tree.

> tree :: Tree String [Int]
> tree = fromList
>   [ ("Frank",   [42])
>   , ("Sascha", [-1])
>   , ("Sandra",  [4,8,15,16,23,42])
>   ]
>
> testLookupFrank, testLookupSandra, testLookupSascha :: IO ()
> testLookupFrank    = runO $ print $ lookup "Frank"   $ observe "tree" tree
> testLookupSandra   = runO $ print $ lookup "Sandra"  $ observe "tree" tree
> testLookupSascha = runO $ print $ lookup "Sascha" $ observe "tree" tree

In order to use this example, we need to define an observe instance for `Char` as well.

> instance Observe Char where
>  obs x = x `seq` o0 x [x]

Now, let's see the tests in action!

    λ> testLookupFrank
    Just [42]
    >>> Observations <<<
    --------------------
    tree
    ----
    (Node (Node _ ((:) (F ) ((:) (r ) ((:) (a ) ((:) (n ) ((:) (k ) ([] )))))) ((:) (42 ) ([] )) _)
          ((:) (S ) _)
          _
          _)

In order to lookup Frank, we only need to take a look at the starting letters `S` and `F` to find `Frank`
on the left branch of the tree.
Of course, we need to compare the key with the whole label to access the corresponding value.

    λ> testLookupSandra
    Just [4,8,15,16,23,42]
    >>> Observations <<<
    --------------------
    tree
    ----
    (Node _
          ((:) (S ) ((:) (a ) ((:) (n ) ((:) (d ) ((:) (r ) ((:) (a ) ([] )))))))
          ((:) (4 ) ((:) (8 ) ((:) (15 ) ((:) (16 ) ((:) (23 ) ((:) (42 ) ([] )))))))
          _)

Sandra is the label of the root, so we only need to take a look at each letter of the key to see if it fits and can then access the corresponding value.

    λ> testLookupSascha
    Just [-1]
    >>> Observations <<<
    --------------------
    tree
    ----
    (Node _
          ((:) (S ) ((:) (a ) ((:) (n ) _)))
          _
          (Node _ ((:) (S ) ((:) (a ) ((:) (s ) ((:) (c ) ((:) (h ) ((:) (a ) ([] ))))))) ((:) (-1 ) ([] )) _))

For Sascha we need to compare the root label "Sandra" until the first character that does not agree with "Sascha", so we inspect the first three characters.
Then we now that we take the right branch of the tree and compute the whole key and associated value.
