> import Prelude hiding (Monoid(..))
>
> class Monoid m where
>     one   :: m
>     (.*.) :: m -> m -> m
>
> -- instance Monoid [a] where
> data Sum = Sum Int deriving (Eq, Show)
>
> instance Monoid Sum where
>     one             = Sum 0
>     Sum x .*. Sum y = Sum (x + y)
>

> instance Monoid () where
>     one     = ()
>     _ .*. _ = ()
>

Beispiel fuer invalide Instanz

> data Zip a = Zip [a] deriving (Eq, Show)
>
> -- instance Monoid m => Monoid (Zip m) where
> --     one               = Zip []
> --     Zip xs .*. Zip ys = Zip (zipWith (.*.) xs ys)
> -- wobei das (.*.) auf der rechten Seiteden Typ (m -> m -> m)

> instance Monoid m => Monoid (Zip m) where
>     one               = Zip (repeat one)
>     Zip xs .*. Zip ys = Zip (zipWith (.*.) xs ys)
> -- wobei das `(.*.)` auf der rechten Seite den Typ `m -> m -> m` hat
> --  und `one` auf der rechten Seite hat den Typ `m`
