> import Control.Applicative (Applicative(..))
> 
> data Expr = Expr :+: Expr
>           | Expr :/: Expr
>           | Num Float
>           | Var Int
> 
> type Env a b = a -> Maybe b
> 
> data ExprEnv a = EE (Env a Float)
> 
> insert :: Eq a => a -> b -> Env a b -> Env a b
> insert k v env = \ k' -> if k == k' then Just v else env k'
> 
> find :: Env a b -> a -> Maybe b
> find = ($)
> 
> insertE :: Eq a => a -> Float -> ExprEnv a -> ExprEnv a
> insertE k v (EE env) = EE (insert k v env)
> 
> findE :: ExprEnv a -> a -> Maybe Float
> findE (EE env) = ($) env
> 
> eEnv :: ExprEnv Int
> eEnv = EE (const Nothing)
> 
> test1 = evalWithEnv eEnv (Num 3 :+: Num 4 )
> -- -> Just 7.0
> test2 = evalWithEnv eEnv (Num 3 :/: (Num (-1) :+: Num 1))
> -- -> Nothing
> test3 = evalWithEnv (insertE 5 3.0 eEnv) (Num 3 :+: Var 5)
> -- -> Just 6.0
> test4 = evalWithEnv (insertE 5 3.0 eEnv) (Num 3 :+: Var 4)
> -- -> Nothing
> 
> evalWithEnv :: ExprEnv Int -> Expr -> Maybe Float
> evalWithEnv env (Var i) = findE env i
> evalWithEnv env (Num n) = pure n
> evalWithEnv env (e1 :+: e2) = pure (+) <*> evalWithEnv env e1 <*> evalWithEnv env e2

It is not possible to handle division by zero with applicative only,
because we cannot change the shape of the container (i.e. `Just` vs `Nothing`)
based on a value.

> evalWithEnv env (e1 :/: e2) = evalWithEnv env e2 >>= \e' ->
>                        case e' of
>                        0 -> Nothing
>                        _ -> pure (/) <*> evalWithEnv env e1 <*> pure e'

A functor (or applicative) instance of `ExprEnv` is not possible to define, because we have nothing a type `a`
  that we might alter by applying `f :: a -> b`.

> -- instance Functor ExprEnv where
> --  fmap f (EE env) =

However, we can define a functor-like type class that we can use for `ExprEnv`.

> class Contravariant f where
>     contramap :: (a -> b) -> f b -> f a

The type class is sometimes also called `ContraFunctor` to emphasise that it's using the idea of a functor type class but applies the function to the contravariant parameter. i.e. the result!

> instance Contravariant ExprEnv where
>     -- env :: b -> Maybe Float  ;  f :: a -> b  ;  x :: a 
>     contramap f (EE env) = EE (\ x -> env (f x))

We can, for example, switch all variable bindings "to the left" by incrementing
  the index that we lookup. 

> test5 = contramap (+ 1) (insertE 5 3.0 eEnv)

λ> findE test5 3
Nothing
λ> findE test5 4
Just 3.0
