Define a `Show`-instance (based on `showsPrec`) for the following enhanced `Expr` data type.
Try to use as less parentheses as possible and document your development with examples.

> data ShowExpr = Binary BinOp ShowExpr ShowExpr
>               | Function Fun ShowExpr
>               | Number Int
>
> data BinOp = Plus | Div
>
> data Fun = SquareRoot

We use this more general approach to define arithmetic expression in order to simplify possible extensions of binary operations and unary functions.

It's quite easy to define the `Show`-instance for operation and function symbols.

> instance Show BinOp where
>    show Div = " / "
>    show Plus = " + "

> instance Show Fun where
>    show SquareRoot = "Sqrt"

Furthermore, we need a helper function that gives us the precedences of operaters in order to handle parentheses correctly.

> precOf :: BinOp -> Int
> precOf Div  = 7
> precOf Plus = 6

As simplification we assume that arguments of unary function only have to be parenthesised if the arguments is not a numeric value, that is, is a composite of expressions.

> isComposite :: ShowExpr -> Bool
> isComposite (Number _) = False
> isComposite _          = True

For the implementation of the `Show`-instance we use parentheses for an expression if the precedence of the surrounding expression is bigger than of the current operator.

> instance Show ShowExpr where
>     showsPrec _ (Number n) = showParen (n < 0) (shows n)
>     showsPrec p (Function f e) =   showString (show f)
>                                  . showChar ' '
>                                  . showParen (isComposite e) (showsPrec p e)
>     showsPrec p (Binary bo e1 e2) =   showParen (p > precOf bo)
>                                     $ showsPrec (precOf bo) e1
>                                     . shows bo
>                                     . showsPrec (precOf bo) e2

The helper function `showParen` is defined as in class, but we use the implicit import of the Prelude.

The actual instance for our original arithmetic expressions (extended by the square root function) as follows.


> data Expr = Num Int
>          | Expr :+: Expr
>          | Expr :/: Expr
>          | Sqrt Expr

> instance Show Expr where
>     show = show . toShowExpr
> 
> toShowExpr (e1 :+: e2) = Binary Plus (toShowExpr e1) (toShowExpr e2)
> toShowExpr (e1 :/: e2) = Binary Div  (toShowExpr e1) (toShowExpr e2)
> toShowExpr (Sqrt e)    = Function SquareRoot (toShowExpr e)
> toShowExpr (Num e)     = Number e

We conclude the exercise with some tests.

> e1, e2, e3, e4, e5, e6 :: Expr
>
> -- 2 + 5 / 7 + 3 / 1
> e1 = Num 2 :+: Num 5 :/: Num 7 :+: Num 3 :/: Num 1
> 
> -- (2 + 5) / 7 + 3 / 1
> e2 = (Num 2 :+: Num 5) :/: Num 7 :+: (Num 3 :/: Num 1)
> 
> -- ((2 + 5) / (7 + 3)) / 1
> e3 = (Num 2 :+: Num 5) :/: (Num 7 :+: Num 3) :/: Num 1
> 
> -- 4 + (-7) / 1
> e4 = Num 4 :+: Num (-7) :/: Num 1
> 
> -- Sqrt (4 + (-7) / 1)
> e5 = Sqrt e4
> 
> -- Sqrt 4
> e6 = Sqrt (Num 4)

ghci> e1
2.0 + 5.0 / 7.0 + 3.0 / 1.0
ghci> e2
(2.0 + 5.0) / 7.0 + 3.0 / 1.0
ghci> e3
(2.0 + 5.0) / (7.0 + 3.0) / 1.0
ghci> e4
4.0 + (-7.0) / 1.0
ghci> e5
Sqrt (4.0 + (-7.0) / 1.0)
ghci> e6
Sqrt 4.0