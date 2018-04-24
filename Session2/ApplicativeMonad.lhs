> data Tree1 a = Leaf a | Branch (Tree1 a) (Tree1 a)
> data Tree2 a = L a | B (Tree2 a) a (Tree2 a)
> data Tree3 a = E | Le a | Br (Tree3 a) (Tree3 a)
>
>
> instance Functor Tree1 where
> instance Applicative Tree1 where
> instance Monad Tree1 where
>     return =  Leaf
>     -- f :: a -> Tree1 b
>     -- x :: a
>     Leaf x >>= f = f x
>     -- t1, t2 :: Tree1 a
>     Branch t1 t2 >>= f = let tNew1 = t1 >>= f
>                              tNew2 = t2 >>= f
>                          in Branch tNew1 tNew2

Not a monad!

> -- instance Functor Tree2 where
> -- instance Applicative Tree2 where
> -- instance Monad Tree2 where
> --     return = L
> --     -- f :: a -> Tree1 b
> --     -- x :: a
> --     L x >>= f = f x
> --     -- t1, t2 :: Tree1 a
> --     B t1 x t2 >>= f = let tNew1 = t1 >>= f
> --                           tNew2 = t2 >>= f
> --                           tCompletelyNew = f x
> --                       in B tNew1 x tNew2

> instance Functor Tree3 where
> instance Applicative Tree3 where
> instance Monad Tree3 where
>     return = Le
>     -- f :: a -> Tree1 b
>     -- x :: a
>     E >>= f = E
>     Le x >>= f = f x
>     -- t1, t2 :: Tree1 a
>     Br t1 t2 >>= f = let tNew1 = t1 >>= f
>                          tNew2 = t2 >>= f
>                      in Br tNew1 tNew2
