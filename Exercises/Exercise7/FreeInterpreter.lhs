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
> freeCalcToExpr :: Free Calc () -> Maybe Expr
> freeCalcToExpr fx = undefined

2. Define an interpreter that evaluate `Free Calc ()` programs directly.

> evalCalc :: Free Calc () -> Maybe Int
> evalCalc fx = undefined

3. Last but not least, define an interpreter to pretty print `Free Calc` programs.

> freeCalcToString :: Free Calc () -> String
> freeCalcToString fx = undefined
