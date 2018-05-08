Define a `Show`-instance (based on `showsPrec`) for the following enhanced `Expr` data type.
Try to use as less parentheses as possible and document your development with examples.

> data ShowExpr = Binary BinOp ShowExpr ShowExpr
>               | Function Fun ShowExpr
>               | Number Double
>
> data BinOp = Plus | Div
>
> data Fun = SquareRoot