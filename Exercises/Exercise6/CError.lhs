In class we defined `CMaybe r a` as a continuation-based version of the `Maybe` monad.
 
1. Implement a data type `CError e r a`{.haskell}, a continuation-based version of the `Error`{.haskell} monad using the same idea. You should use the [module MonadError](http://www-ps.informatik.uni-kiel.de/~nda/MonadError.hs) and give corresponding instances for `Monad` and `MonadError`.

> {-# Language MultiParamTypeClasses, FlexibleInstances #-}
> module CError where

> import Exercises.Exercise6.MonadError
> import Control.Monad (ap)

> -- you should change this data type definition, as this is only given in order to make the file compile
> newtype CError e r a =
>   CError { handler :: (e -> Error e r) -> (a -> Error e r) -> Error e r }

> instance Functor (CError e r) where
>     fmap f (CError ce) =
>         CError (\eh rh -> ce eh (\r -> handler (return (f r)) eh rh))

> instance Applicative (CError e r) where
>     pure = return
>     (<*>) = ap

In the definition of the monad instance, we only need to propagate the error handler, whereas the other continuation is applied.
If you ignore all passing of `eh`, you get the implementation of `CMaybe`.

> instance Monad (CError e r) where
>     return x        = CError (\_ rh -> rh x)
>     CError ce >>= f = CError (\eh rh -> ce eh (\r -> handler (f r) eh rh))

For the MonadError instance the implementation behaves dual to the monad instance.
We propagate success continuations and apply the error handler.
   
> instance MonadError e (CError e r) where
>    throw e             = CError (\eh _ -> eh e)
>    CError ce `catch` f = CError (\eh rh -> ce (\e -> handler (f e) eh rh) rh)
    
2. Give an implementation to transform the continuated-based error handler into an ordinary error handler.

> runCError :: CError e r r -> Error e r
> runCError ce = handler ce Thrown Result

Such constructions are sometimes called *free*, because we substitute abstract operations with concrete constructors.

3.  Adapt the evaluation function defined in `MonadError` to use `CError`.

> simpleEval :: Expr -> Error ExprEvalError Double
> simpleEval = runCError . eval

The actual computations stay the same, we just have to give the stretegy we want to evaluate the given `CError` compute to an `Error` value.

λ> CError.simpleEval sqrtNeg
Thrown Root of a negative number
λ> CError.simpleEval divByZero
Thrown Division by zero
λ> CError.simpleEval sixteenToNine
Result 1.7777777777777777
