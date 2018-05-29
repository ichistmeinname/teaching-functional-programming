> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> import Data.Functor.Identity (Identity(..))
> import Free

In class we discussed that we can transform the `Delay` monad into `Free Identity` and vice versa as well as the `Maybe` monad into `Free One`.

> class Iso t1 t2 where
>     to :: t1 -> t2
>     from :: t2 -> t1

1. Give an implementation for the instance `Iso (Delay a) (Free Identity a)`.

> data Delay a = Now a
>              | Later (Delay a)

> instance Iso (Delay a) (Free Identity a) where
>     to   (Now x)                = Pure x
>     to   (Later dx)             = Impure (Identity (to dx))
>     from (Pure x)               = Now x
>     from (Impure (Identity fx)) = Later (from fx)

2. Give an implementation for the instance `Iso (Maybe a) (Free One a)`.

> data One a = One

> instance Iso (Maybe a) (Free One a) where
>   to   (Just x)    = Pure x
>   to   Nothing     = Impure One
>   from (Pure x)    = Just x
>   from (Impure fx) = Nothing

3. Prove that the round-trip property for `to` and `from` hold for both instances!

  Delay
  ======

    (a) forall types (a :: *) and (fx :: Delay a), we want to show

       to (from fx) = fx
    
    By using induction on `fx`, we can reason as follows.
    
    (1) fx = Now x

      from (to (Now x))
    = from (Pure x)
    = Now x

    (2) fx = Later fx' and induction hypothesis `from (to fx') = fx'

      from (to (Later fx')
    = from (Impure (Identity (to fx')))
    = Later (from (to fx'))
    = Later fx'


    (b) forall types (a :: *) and (fx :: Free Identity a), we want to show

       to (from fx) = fx
    
    By using induction on `fx`, we can reason as follows.

    (1) fx = Pure x

      to (from (Pure x))
    = to (Now x)
    = Pure x

    (2) fx = Impure (Identity fx') and induction hypothesis `to (from fx') = fx'

      to (from (Impure (Identity fx')))
    = to (Later (from fx'))
    = Impure (Identity (to (from fx')))
    = Impure Identity fx'


  Maybe
  =====
  
    (a) forall types (a :: *) and (mx :: Maybe a), we want to show

       to (from mx) = mx
    
    By using induction on `fx`, we can reason as follows.
    
    (1) fx = Just x

      from (to (Just x))
    = from (Pure x)
    = Just x

    (2) fx = Nothing

      from (to Nothing)
    = from (Impure One)
    = Nothing


    (b) forall types (a :: *) and (fx :: Free One a), we want to show

       to (from fx) = fx
    
    By using induction on `fx`, we can reason as follows.

    (1) fx = Pure x

      to (from (Pure x))
    = to (Just x)
    = Pure x

    (2) fx = Impure One

      to (from (Impure One))
    = to Nothing
    = Impure One
