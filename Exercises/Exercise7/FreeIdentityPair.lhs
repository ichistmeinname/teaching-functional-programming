> import Free

First, we want to search for the functor in order to get an instantiation of `Free f a` that is isomorphic to `Identity a`.

1. Give an implementation of the corresponding `Iso`-instance.
2. Prove the round-trip property for `to` and `from`.

Second, we search for the monad (aka. data type that is an instance of `Monad`) that is isomorphic to `Free Pair a`, where `Pair` is defined as follows.

> data Pair a = Pair a a

3. Give the functor instance for `Pair`.
4. Implement the `Iso`-instance for `Free Pair a` and the corresponding monad.
5. Prove the round-trip property.
