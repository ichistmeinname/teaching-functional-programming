> import Prelude hiding (Monad(..), Functor(..))
> import Data.Functor.Identity (Identity(..))
>     
> class Monad m where
>     (>>=)  :: m a -> (a -> m b) -> m b
>     return :: a -> m a

> class Functor f where
>     fmap :: (a -> b) -> f a -> f b

> fmapM :: Monad m => (a -> b) -> m a -> m b
> -- f :: (a -> b)
> -- mx :: m a
> -- my :: m b
> fmapM f mx = my
>  -- x :: a
>  -- f x :: b
>  -- return (f x) :: m b
>  where my = mx >>= \x -> return (f x)

> class Functor f => Applicative f where
>     (<*>) :: f (a -> b) -> f a -> f b
>     pure :: a -> f a

> -- (<*>) from Applicative
> ap :: MonadAlt m => m (a -> b) -> m a -> m b
> -- f :: (a -> b)
> -- x :: a
> -- f x :: b
> -- mx :: m a 
> -- my = fmap (\x -> f x) mx :: m b
> -- fmap (\f -> fmap (\x -> f x ) mx) mf :: m (m b)
> ap mf mx = join (fmap (\f -> fmap (\x -> f x ) mx) mf)

> class Functor m => MonadAlt m where
>     returnAlt :: a -> m a
>     join :: m (m a) -> m a

> bind :: MonadAlt m => m a -> (a -> m b) -> m b
> -- mx :: m a
> -- f :: a -> n b
> -- fmap f mx :: m (n b)
> bind mx f = join (fmap f mx)
>
> fmapJoin :: MonadAlt m => (a -> b) -> m a -> m b
> fmapJoin f mx = undefined
>

With the condition that f is a Functor!

> data AMonad a = Return a
>               | Join (AMonad (AMonad a))

> instance Functor AMonad where
>   -- f :: (a -> b)
>   -- x :: a
>   fmap f (Return x) = Return (f x)
>   -- mx :: AMonad f (AMonad f a)
>   -- fmap f mx :: <type_error>
>   -- my :: AMonad f a
>   -- mz :: AMonad f b
>   fmap f (Join mx)  =
>       Join (fmap (\ my -> fmap (\y -> f y) my) mx)
> --    Join (fmap (fmap f) mx)

> val1 :: AMonad ()
> val1 = Return ()
>
> val2 :: AMonad Int
> val2 = Return 0
>
> val3 :: AMonad Int
> val3 = Join (Return (Return 0))

> val4 :: AMonad (Maybe Int)
> val4 = Return (Just 42)
>
> val5 :: AMonad (Maybe Int)
> val5 = Return Nothing
>
> -- val6 :: AMonad (Maybe Int)
> -- val6 = fmap (fmap (+1)) val4

> -- data AMonad f a = Return a
> --                 | Join (f (AMonad f a))

The type parameter `f` has to be a Functor!

> data Free f a = Pure a
>               | Impure (f (Free f a))

> valF1 :: Free f ()
> valF1 = Pure ()
>
> valF2 :: Free f ()
> valF2 = Impure fx
>  where fx :: f (Free f ())
>        fx = undefined

> valF3 :: Free Identity ()
> valF3 = Impure fx
>  where fx :: Identity (Free Identity ())
>        -- fx = Identity (Pure ())
>        fx = Identity valF1
>
> valF4 :: Free Identity ()
> valF4 = Impure (Identity valF3)
>
> -- valF1
> -- Pure ()
>
> -- valFN
> -- Impure (Identity (Impure ... (... ())))
>            ----------------
> --         repeat (n-1) times

These values look kind of familiar.

> data Nat = Zero
>          | Succ Nat
>
> toNat :: Free Identity () -> Nat
> toNat (Pure ()) = Zero
> toNat (Impure (Identity fx)) = Succ (toNat fx)
>
> fromNat :: Nat -> Free Identity ()
> fromNat Zero = Pure ()
> fromNat (Succ n) = Impure (Identity (fromNat n))

        toNat . fromNat = id
        fromNat . toNat = id

> valF6 :: Free Identity Int
> valF6 = Impure (Identity (Impure (Identity (Pure 42)))) 

> instance Functor Identity where
>   fmap f (Identity x) = Identity (f x)

Something that is isomorphic to `Free Identity a`

> data Delay a = Now a
>              | Later (Delay a)
>  deriving Show
>
> toDelay :: Free Identity a -> Delay a
> toDelay (Pure x) = Now x
> toDelay (Impure (Identity fx)) = Later (toDelay fx)

> instance Functor f => Monad (Free f) where
>     return   = Pure
>     -- f :: a -> Free f b
>     -- x :: a
>     Pure x >>= f = f x
>     -- fx :: f (Free f a)
>     -- f :: a -> Free f b
>     -- fy :: Free f a
>     -- x :: a
>     -- g :: Free f a -> Free f b
>     Impure fx >>= f =
>         Impure (fmap (\fy -> g fy) fx)
>      where g fz = fz >>= f

> -- data Free f a = Pure a
> --               | Impure (f (Free f a))

--
-- Just 42
-- Just ()

> just :: a -> Free MaybeFunctor a
> just = Pure

-- Nothing

> nothing :: Free MaybeFunctor a
> nothing = Impure fx
>  where
>    fx :: MaybeFunctor (Free MaybeFunctor a)
>    fx = One

> type MaybeFunctor = One
> data One a = One
> instance Functor One where
>     fmap f One = One

-- AMonad f  ~=~ Functor f => f a
-- Return :: a -> AMonad f a
-- Join   :: AMonad f (AMonad f a) -> AMonad f a

-- Return :: a -> f a
-- Join   :: f (f a) -> f a

> -- type ExampleMonad a = AMonad Maybe a

> data SWTC tc a = C1 (tc ()) | C2 a
