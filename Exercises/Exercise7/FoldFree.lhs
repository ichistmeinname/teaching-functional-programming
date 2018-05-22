> {-# LANGUAGE RankNTypes #-}

> import Free

We are quite familiar with folding functions for lists and might still remember folding on trees from our Bachelors' course on `Advanced Programming`.

> data List a = Nil | Cons a (List a)
>
> foldList :: (a -> b -> b) -> b -> List a -> b
> foldList cons nil xs = case xs of
>                          Nil -> nil
>                          Cons y ys -> cons y (foldList cons nil ys)

> data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)
>
> foldTree :: (b -> b -> b) -> (a -> b) -> b -> Tree a -> b
> foldTree branch leaf empty tree = case tree of
>                                      Empty -> empty
>                                      Leaf x -> leaf x
>                                      Branch t1 t2 -> branch (foldTree branch leaf empty t1) (foldTree branch leaf empty t2)

The naming was chosen to make the essense of a folding function more clear: for each constructor of the data type, the folding function takes an argument that can transform the constructor into a value of type `b`.
That is, each concrete constructor is then replaced by its abstract interpretation.

1. Implement a folding function `foldFree` for the data type `Free f a`.

> foldFree = undefined

2. All previous implementation that transform `Free f a` into a concrete monad can now be implemented using `foldFree`. Just do it!
3. We can even observe a special case. If the target type `b` is a monad, we can define a function `induce` that takes a so-called natural transformation and transforms the free value into a monad. Now use `induce` to define the transformations function one last time.

> type Natural f m a = forall a. f a -> m a
>
> induce :: Monad m => Natural f m a -> Free f a -> m a
> induce ntf fx = foldFree undefined undefined fx
    
4. Explain why it is not enough to quantify the type variable `a` in the type signature of `induce` at the beginning --- as we normally do, but quantify over all type variables `a` directly in the function type of the first arugment.
