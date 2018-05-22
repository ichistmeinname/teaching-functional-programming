In class we defined `CMaybe r a` as a continuation-based version of the `Maybe` monad.
 
1. Implement a data type `CError e r a`{.haskell}, a continuation-based version of the `Error`{.haskell} monad using the same idea. You should use the [module MonadError](http://www-ps.informatik.uni-kiel.de/~nda/MonadError.hs) and give corresponding instances for `Monad` and `MonadError`.

> {-# Language MultiParamTypeClasses, FlexibleInstances #-}
> module CError where

> import MonadError

> -- you should change this data type definition, as this is only given in order to make the file compile
>

data Maybe a = Nothing
             | Just a

Nothing :: Maybe a
Just :: a -> Maybe a

newtype CMaybe r a = CMaybe ((a -> Maybe r) -> Maybe r)

data Error err a = Thrown err | Result res
    deriving Show

Result :: a -> Error err a
Thrown :: err -> Error err a 

> newtype CError e r a = CError ((a -> Error e r) -> (e -> Error e r) -> Error e r)

CError :: ((a -> Error e r) -> (e -> Error e r) -> Error e r) -> CError e r a

> instance Functor (CError e r) where
>     fmap f (CError handler) = CError (\rh eh -> handler (\x -> rh (f x)) eh)
>     -- fmap :: (a -> b) -> CError e r a -> CError e r b
>     -- rh :: b -> Error e r
>     -- eh :: e -> Error e r
>     -- f :: (a -> b)
>     -- handler :: (a -> Error e r) -> (e -> Error e r) -> Error e r
>     --fmap f (CError handler) = CError (\rh eh -> aValue rh eh)
>     -- where
>     --   aValue :: (a -> Error e r) -> (e -> Error e r) -> Error e r
>     --   -- x :: a
>     --   -- err :: e
>     --   aValue rh eh = handler (\x -> anError1 rh x) (\err -> anError2 eh err)
>     --   anError1 :: (a -> Error e r) -> a -> Error e r
>     --   anError1 rh x = rh (f x)
>     --   -- anError1 = rh . f
>     --   anError2 :: (e -> Error e r) -> e -> Error e r
>     --   anError2 eh err = eh err

> instance Applicative (CError e r) where
>     
> instance Monad (CError e r) where
>     -- x :: a
>     -- rh :: (a -> Error e r)
>     -- eh :: (e -> Error e r)
>     return x = CError (\rh eh -> rh x)
>     -- handler :: (a -> Error e r) -> (e -> Error e r) -> Error e r
>     -- f :: a -> CError e r b
>     -- rh :: b -> Error e r
>     -- eh :: e -> Error e r
>     -- handler2 :: (b -> Error e r) -> (e -> Error e r) -> Error e r
>     CError handler >>= f =
>         CError (\rh eh -> handler (\x -> let CError handler2 = f x
>                                          in handler2 rh eh) eh)

> instance MonadError e (CError e r) where
>     -- throw :: e -> CError e r a
>     throw err = CError (\rh eh -> eh err)
>     -- handler :: (a -> Error e r) -> (e -> Error e r) -> Error e r
>     -- f :: e -> CError e r a
>     -- rh :: a -> Error e r
>     -- eh :: e -> Error e r
>     -- handler2 :: (a -> Error e r) -> (e -> Error e r) -> Error e r
>     CError handler `catch` f =
>         CError (\rh eh -> handler rh (\err -> let CError handler2 = f err
>                                               in handler2 rh eh))

In order to handle errors your data type should have two continuations: one to take in case of success and one to take in case of an error.
    
2. Give an implementation to transform the continuated-based error handler into an ordinary error handler.

> runCError :: CError e r r -> Error e r
> runCError (CError handler) = handler Result Thrown

3.  Adapt the evaluation function defined in `MonadError` to use `CError`.

eval :: MonadError ExprEvalError m => Expr -> m Double

> evalCPS :: Expr -> Error ExprEvalError Double
> evalCPS = runCError . eval

sixteenToNine :: Expr
sixteenToNine = (Num 5 :+: Num 11) :/: Num 9

divByZero :: Expr
divByZero = (Sqrt (Num 16) :+: Num 8) :/: (Num (-1) :+: Num 1)

sqrtNeg :: Expr
sqrtNeg = Sqrt (Num 3 :+: (Num 5 :/: Num (-1)))

