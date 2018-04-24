Let's reconsider our implementation of arithmetic expressions and the corresponding evaluation function once again. Last week we added variables and an environment to lookup (numeric) values and tried to use applicative combinators only. In order to handle division with zero, applicative does not suffice, we need the monadic combinator (>>=) to indicate a failure although we have actual values at hand.

So, the evaluator with monadic combinators (where needed) and an environment looks as follows.

> import Control.Applicative (Applicative(..))
> 
> type Env a b = a -> Maybe b
> 
> type ExprEnv a = Env a Float
> 
> insert :: Eq a => a -> b -> Env a b -> Env a b
> insert k v env = \ k' -> if k == k' then Just v else env k'
> 
> find :: Env a b -> a -> Maybe b
> find = ($)

> eval :: Env a Float -> Expr a -> Maybe Float
> eval env (Var i) = env i
> eval env (Num n) = pure n
> eval env (e1 :+: e2) = pure (+) <*> eval env e1 <*> eval env e2
> eval env (e1 :/: e2) = eval env e2 >>= \e' ->
>                        case e' of
>                        0 -> Nothing
>                        _ -> pure (+) <*> eval env e1 <*> pure e'

Refactor the code, such that the environment is hidden in a state monad and, in order to have a more sophisticated usage of the environment, add a construct for let-bindings to your expression data type. Furthermore, the expression type is parametrised over the type to represent variable bindings.

> data Expr a = Expr a :+: Expr a
>             | Expr a :/: Expr a
>             | Num Float
>             | Var a
>             | Let a Float
