Let's define the following data types

> data Composed f g a = Comp { composed :: f (g a) }
> data Product  f g a = Prod { product  :: (f a, g a) }

to represent the composition and multiplication of type constructors.

Implement the following Applicative instances.

> instance (Applicative f, Applicative g) => Applicative (Composed f g) where

> instance (Applicative f, Applicative g) => Applicative (Product f g) where

Do your implementations obey the corresponding laws?
Is it possible to define a instance of Monad for the data type Composed?
If so, give an implementation, otherwise explain why this is not possible.
How about Product?
