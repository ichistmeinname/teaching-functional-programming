Let's reconsider our implementation of arithmetic expressions and the corresponding evaluation function once again. Last week we added variables and an environment to lookup (numeric) values and tried to use applicative combinators only. In order to handle division with zero, applicative does not suffice, we need the monadic combinator (>>=) to indicate a failure although we have actual values at hand.

So, the evaluator with monadic combinators (where needed) and an environment looks as follows.

> import Control.Applicative (Applicative(..))
> import Control.Monad.Trans.State
>
> type Env a b = a -> Maybe b
> 
> data ExprEnv a = EEnv (Env a Float)
> 
> insert :: Eq a => a -> b -> Env a b -> Env a b
> insert k v env = \ k' -> if k == k' then Just v else env k'
> 
> find :: Env a b -> a -> Maybe b
> find = ($)

> insertE :: Eq a => a -> Float -> ExprEnv a -> ExprEnv a
> insertE k v (EEnv env) = EEnv (insert k v env)
> 
> findE :: ExprEnv a -> a -> Maybe Float
> findE (EEnv env) k = env k

Refactor the code, such that the environment is hidden in a state monad and, in order to have a more sophisticated usage of the environment, add a construct for let-bindings to your expression data type. Furthermore, the expression type is parametrised over the type to represent variable bindings.

> data Expr a = Expr a :+: Expr a
>             | Expr a :/: Expr a
>             | Num Float
>             | Var a
>             | Let a Float (Expr a)

First we add the new rule for let-expressions.

> eval :: Eq a => ExprEnv a -> Expr a -> Maybe Float
> eval env (Var i) = findE env i
> eval env (Num n) = pure n
> eval env (e1 :+: e2) = pure (+) <*> eval env e1 <*> eval env e2
> eval env (e1 :/: e2) = eval env e2 >>= \e' ->
>                        case e' of
>                        0 -> Nothing
>                        _ -> pure (/) <*> eval env e1 <*> pure e'
> eval env (Let x v expr) = eval (insertE x v env) expr

In a second step we change the code to use a state monad.

> type ExprState a = State (ExprEnv a) (Maybe Float)

> evalS :: Eq a => Expr a -> ExprState a
> evalS (Var i) = get >>= \env -> return (findE env i)
> evalS (Num n) = return (pure n)
> evalS (e1 :+: e2) =
>   evalS e1 >>= \e1' ->
>   evalS e2 >>= \e2' ->
>   return (pure (+) <*> e1' <*> e2')
> evalS (e1 :/: e2) =
>   evalS e2 >>= \mx ->
>   evalS e1 >>= \my ->
>   return (mx >>= \e' ->
>           case e' of
>             0 -> Nothing
>             _ -> pure (/) <*> my <*> mx)
> evalS (Let x v expr) = get >>= \env -> put (insertE x v env) >> evalS expr

At last, we define some arithmetic expressions to test our setup.

> testExpr1 :: Expr Char
> testExpr1 = Let 'x' 1.0 (Num 42.0 :+: (Num 1.0 :/: Var 'x'))

> testExpr2 :: Expr Char
> testExpr2 = Let 'x' 0.0 (Num 42.0 :+: (Num 1.0 :/: Var 'x'))

> emptyEEnv :: ExprEnv a
> emptyEEnv = EEnv (const Nothing)
>
> testEvalS :: IO ()
> testEvalS = mapM_ go [(testExpr1,"ex1"), (testExpr2,"ex2")]
>  where
>    go (testExpr,name) =
>      putStrLn (name ++ ": " ++ show (evalState (evalS testExpr) emptyEEnv))

~~~~~~~~ Optional remarks for interested readers ~~~~~~~~
The attentive programmer might observe that we need to handle two different effects in the function evalS: state and maybe!
These effects can interact more closely than we have implemented it above.
In case of a failed computation, i.e. `Nothing`, it does not make that much sense to still propagate the environment in the second component. That is, instead of yielding a value of type `(Maybe Float, ExprEnv a)` its more convenient to stack the effects as `Maybe (Float, ExprEnv a)`.

> type ExprState' a = ExprEnv a -> Maybe (Float, ExprEnv a)

Fortunately, the Haskell eco-system already provides the right data structure to handle such a case: a state transformer!

λ> :i StateT
newtype StateT s (m :: * -> *) a
  = StateT {runStateT :: s -> m (a, s)}
  	-- Defined in ‘Control.Monad.Trans.State.Lazy’

> type ExprST a = StateT (ExprEnv a) Maybe Float

> evalST :: Eq a => Expr a -> ExprST a
> evalST (Var i) = findST i
> evalST (Num n) = return n
> evalST (e1 :+: e2) =
>   evalST e1 >>= \e1' ->
>   evalST e2 >>= \e2' ->
>   return (e1' + e2')
> evalST (e1 :/: e2) =
>   evalST e2 >>= \e2 ->
>   case e2 of
>     -- here, it's not sufficient to handle both effects simulatenously, we need
>     --  a more fine-grained handling, because we want to yield `Nothing`
>     --  although the computation alone did not yield any error
>     -- so, in the end, it's maybe better to use the above solution with
>     --  nested monads after all.
>     0 -> StateT (const Nothing)
>     _ -> evalST e1 >>= \e1 -> return (e1 / e2)
> evalST (Let x v expr) = get >>= \env -> put (insertE x v env) >> evalST expr

> findST :: a -> ExprST a
> findST k = StateT (\env -> fmap (\x -> (x,env)) (findE env k))

> testEvalST :: IO ()
> testEvalST = mapM_ go [(testExpr1,"ex1"), (testExpr2,"ex2")]
>  where
>    go (testExpr,name) =
>      putStrLn (name ++ ": " ++ show (evalStateT (evalST testExpr) emptyEEnv))
