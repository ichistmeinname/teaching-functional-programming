> import Prelude hiding (Applicative(..))
> import Data.Char (ord)

> data Expr a = Expr a :+: Expr a
>             | Expr a :/: Expr a
>             | Num Float
>             | Var a

> type Env a b = a -> Maybe b
> 
> add :: Eq a => a -> b -> Env a b -> Env a b
> add k v env = \k' -> if k == k' then Just v else env k'
> 
> find :: ExprEnv a -> a -> Maybe Float
> find (EE env) k = env k
>
> --   x + 4              | x |-> 5
> -- = 9
> expr1 :: Expr Int
> expr1 = Var 1 :+: Num 4
>
> expr2 :: Expr Int
> expr2 = Var 1 :/: Num 0

> class Applicative f where
>     pure :: a -> f a
>     (<*>) :: f (a -> b) -> f a -> f b

> instance Applicative Maybe where
>     pure = Just
>     Just f <*> Just x = Just (f x)
>     _      <*> _      = Nothing
>
> evalWithEnv :: ExprEnv a -> Expr a -> Maybe Float
> evalWithEnv env (Num v) = Just v
> evalWithEnv env (Var i) = find env i
> evalWithEnv env (e1 :+: e2) =
>   pure (+) <*> evalWithEnv env e1 <*> evalWithEnv env e2
> evalWithEnv env (e1 :/: e2) =
> --  pure (\ x y -> if y == 0 then Nothing else Just (x / y)) <*>
> --       evalWithEnv env e1 <*>
> --       evalWithEnv env e2
> -- pure (\ x y -> if y == 0 then Nothing else Just (x / y))
> --   :: f (a -> b -> Maybe c)

>   evalWithEnv env e1 >>= \ x ->
>   evalWithEnv env e2 >>= \ y ->
>     if y == 0 then Nothing else return (x / y)
> --  case y of     
> --    0 -> Nothing
> --    _ -> return (x / y)

> --    case evalWithEnv env e2 of
> --      Nothing -> Nothing
> --      Just 0 -> Nothing
> --      Just n -> pure (/) <*> evalWithEnv env e1 <*> pure n

(>>=) :: Monad m => m a -> (a -> m b) -> m b

(<*>) :: f (a -> b) -> f a -> f b

pure (+) :: f (a1 -> (a1 -> a1))
~> a |-> a1     b |-> a1 -> a1

(pure (+) <*>) :: f a1 -> f (a1 -> a1)

> data ExprEnv a = EE (Env a Float)

We cannot define a valid functor instance.

> instance Functor ExprEnv where
>     -- fmap :: (a -> b) -> ExprEnv a -> ExprEnv b
>     -- "~>" fmap :: (a -> b) -> (a -> Maybe Float)
>     --                       -> (b -> Maybe Float)
>     -- env :: a -> Maybe Float
>     -- f :: a -> b
>     -- y :: b
>     fmap f (EE env) = EE (\y -> Nothing)
>
> data Predicate a = Pred (a -> Bool)

> -- instance Functor Predicate where
>
>
> -- manchmal auch CoFunctor, manchmal ContraFunctor
> class Contravariant f where
>     contramap :: (a -> b) -> f b -> f a
>
> instance Contravariant ExprEnv where
>     -- f :: a -> b
>     -- env :: b -> Maybe Float
>     -- e :: a -> Maybe Float
>     contramap f (EE env) = EE e
>       where e = \x -> env (f x)

> testEnv :: ExprEnv Int
> testEnv = EE (\k -> if k == 1 then Just 5 else Nothing)

> class CoFunctor f where
>     -- fmap :: (a -> b) -> (f a -> f b)
>     comap :: (b -> a) -> (f b -> f a)
>
