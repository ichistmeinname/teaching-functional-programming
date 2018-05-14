In class we defined `CMaybe r a` as a continuation-based version of the `Maybe` monad.
 
1. Implement a data type `CError e r a`{.haskell}, a continuation-based version of the `Error`{.haskell} monad using the same idea. You should use the [module MonadError](http://www-ps.informatik.uni-kiel.de/~nda/MonadError.hs) and give corresponding instances for `Monad` and `MonadError`.

> {-# Language MultiParamTypeClasses, FlexibleInstances #-}
> module CError where

> import Exercises.Exercise6.MonadError

> -- you should change this data type definition, as this is only given in order to make the file compile
> newtype CError e r a = CError { stupid :: Bool }

> instance Functor (CError e r) where
> instance Applicative (CError e r) where
> instance Monad (CError e r) where

> instance MonadError e (CError e r) where

In order to handle errors your data type should have two continuations: one to take in case of success and one to take in case of an error.
    
2. Give an implementation to transform the continuated-based error handler into an ordinary error handler.

> runCError :: CError e r r -> Error e r
> runCError = undefined

3.  Adapt the evaluation function defined in `MonadError` to use `CError`.
