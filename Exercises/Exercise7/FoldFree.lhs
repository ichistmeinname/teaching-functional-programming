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

> foldFree :: Functor f => (f b -> b) -> (a -> b) -> Free f a -> b
> foldFree impure pure (Pure x) = pure x
> foldFree impure pure (Impure fx) = impure (fmap (foldFree impure pure) fx)

2. All previous implementation that transform `Free f a` into a concrete monad can now be implemented using `foldFree`. Just do it!
3. We can even observe a special case. If the target type `b` is a monad, we can define a function `induce` that takes a so-called natural transformation and transforms the free value into a monad. Now use `induce` to define the transformations function one last time.

> type Natural f m = forall a. f a -> m a
>
> induce :: (Functor f, Monad m) => Natural f m -> Free f a -> m a
> induce ntf fx = foldFree ((>>= id) . ntf) return fx

4. Explain why it is not enough to quantify the type variable `a` in the type signature of `induce` at the beginning --- as we normally do, but quantify over all type variables `a` directly in the function type of the first arugment.

If we try to define the function with a less general type, we get the following error message.

  > induce' :: (Functor f, Monad m) => (f a -> m a) -> Free f a -> m a
  > induce' ntf fx = foldFree ((>>= id) . ntf) return fx

• Couldn't match type ‘a’ with ‘m a’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          induce' :: forall (f :: * -> *) (m :: * -> *) a.
                     (Functor f, Monad m) =>
                     (f a -> m a) -> Free f a -> m a
        at FoldFree.lhs:41:14
      Expected type: f a -> m (m a)
        Actual type: f a -> m a
• In the second argument of ‘(.)’, namely ‘ntf’
  In the first argument of ‘foldFree’, namely ‘((>>= id) . ntf)’
  In the expression: foldFree ((>>= id) . ntf) return fx

So, what's the problem here?
In order to use `foldFree` we have to supply a function of type `impure :: f b -> b` if we want to fold a `Free f a` to a value of type `b`.
In case of `induce` our target type is `m a`, that is, the type of function we need to pass becomes `impure_induce :: f (m a) -> m a`.
That is, to use `foldFree` we need a more general type that can transform any type within the `f` into a corresponding `m`.
So, we need to quantify the type variable as we do in the type synonym for `Natural`.
In order to do this, we enable the language extension for `RankNTypes`, whereas `Rank2Types` would have sufficed as well.
