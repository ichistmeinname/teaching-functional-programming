> data Composed f g a = Comp { composed :: f (g a) }
> data Product  f g a = Prod { product  :: (f a, g a)}

to represent the composition and multiplication of type constructors.

(1) Before we implement the `Applicative` instances, we define the `Functor` instances first.

In case of `Composed`, we just propagate the function `f` to the underlying functors.

> instance (Functor f, Functor g) => Functor (Composed f g) where
>     -- f :: (a -> b)
>     -- fgx :: f (g a)
>     fmap f (Comp fgx) = Comp (fmap (fmap f) fgx)

In case of `Product` the implementation is even simpler, we just use `fmap` on both components.

> instance (Functor f, Functor g) => Functor (Product f g) where
>     fmap f (Prod (x, y)) = Prod (fmap f x, fmap f y)
>
> instance (Applicative f, Applicative g) => Applicative (Product f g) where
>     pure x = Prod (pure x, pure x)
>     -- fx :: f (a -> b)
>     -- fy :: g (a -> b)
>     -- x :: f a
>     -- y :: g a
>     Prod (fx,fy) <*> Prod (x,y) = Prod (v,w)
>      -- v :: f b
>      -- w :: g b
>      where v = fx <*> x
>            w = fy <*> y

> instance (Applicative f, Applicative g) => Applicative (Composed f g) where
>     -- x :: a
>     -- fgx :: f (g a)
>     pure x = Comp fgx
>      where fgx = pure (pure x)
>     -- fgf :: f (g (a -> b))
>     -- fgx :: f (g a)
>     Comp fgf <*> Comp fgx = Comp fgy
>      -- fgy :: f (g b)
>                  -- f (f (g b))
>      where fgy = (\gf gx -> gf <*> gx) <$> fgf <*> fgx 
>            -- fmap (\gf -> (\gx f -> f <$> gx) <$> fgx <*> gf) fgf
>            -- gy = gf <*> gx
>            -- gf :: g (a -> b)
>            -- gx :: g a
>            -- gy :: g b

