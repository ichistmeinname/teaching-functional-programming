> import Free

We have used arithmetic expression as example for nearly all topics introduced in the lecture. Free monads is no exception.

One key essence of using a `Free` is that we have an AST-like representatin of a monadic program. The computations are not actually performed, but stacked via the `Impure`-constructor. That is, when we represent a monadic program using `Free` we can implement various interpreters for this program.

Here, we want to talke arithmetic expressions, again. In order to have a monadic program with arithmetic expression, we define a calculator-like DSL.

> data Calc next = Add next
>                | Num Int next
>                | Clear next
>                | End

The type parameter `next` is used to chain commands and th operation `End` marks the end of such a chain (similar to pushing the "="-button on most common calculators). 

With this DSL on top of `Free` we can define programs as follows.

> instance Functor Calc where
>     fmap f (Add r) = Add (f r)
>     fmap f (Num x r) = Num x (f r)
>     fmap f (Clear r) = Clear (f r)
>     fmap _ End = End

> liftFree :: Functor f => f a -> Free f a
> liftFree fx = Impure (fmap return fx)
>
> num :: Int -> Free Calc ()
> num x = liftFree (Num x ())

> add :: Free Calc ()
> add = liftFree (Add ())
>
> clear :: Free Calc ()
> clear = liftFree (Clear ())
>
> end :: Free Calc ()
> end = liftFree End

> program1 :: Free Calc ()
> program1 = do
>   num 1
>   num 2
>   add
>   end

> program2 :: Free Calc ()
> program2 = do
>   num 1
>   num 2
>   add
>   clear
>   num 42
>   end

Using `foldFree` we can define intepretations of calculator-programs.

1. Define an interpreter that transforms the `Free`-based program into our well-known `Expr` data type.

> data Expr = Number Int
>           | Expr :+: Expr
>           | Expr :*: Expr
>  deriving Show

> calcToExpr :: Calc ([Expr] -> Maybe Expr) -> [Expr] -> Maybe Expr
> calcToExpr (Num x r) es = r (Number x : es)
> calcToExpr (Add r) (x:y:es) = r (x :+: y:es)
> calcToExpr (Clear r) _ = r []
> calcToExpr End [result] = Just result
> calcToExpr _ _ = Nothing

> freeCalcToExpr :: Free Calc () -> Maybe Expr
> freeCalcToExpr fx = foldFree calcToExpr (\_ _ -> Nothing) fx []

2. Define an interpreter that evaluate `Free Calc ()` programs directly.

> calcToMInt :: Calc ([Int] -> Maybe Int) -> [Int] -> Maybe Int
> calcToMInt (Num x r) es = r (x : es)
> calcToMInt (Add r) (x:y:es) = r (x + y:es)
> calcToMInt (Clear r) _ = r []
> calcToMInt End [result] = Just result
> calcToMInt _ _ = Nothing

> evalCalc :: Free Calc () -> Maybe Int
> evalCalc fx = foldFree calcToMInt (\_ _ -> Nothing) fx []

3. Last but not least, define an interpreter to pretty print `Free Calc` programs.

> calcToString :: Calc ([String] -> String) -> [String] -> String
> calcToString (Num x r) acc       = r (show x : acc)
> calcToString (Add r)   (x:y:acc) = r (x:"+":y:acc)
> calcToString (Clear r) acc = r []
> calcToString End       acc = concat acc

> freeCalcToString :: Free Calc () -> String
> freeCalcToString fx = foldFree calcToString (\_ _ -> "") fx []

Some tests for the functions above.
  
λ> freeCalcToString program1
"2+1"
λ> freeCalcToExpr program1
Just (Number 2 :+: Number 1)
λ> evalCalc program1
Just 3
λ> freeCalcToString program2
"42"
λ> freeCalcToExpr program2
Just (Number 42)
λ> evalCalc program2
Just 42
