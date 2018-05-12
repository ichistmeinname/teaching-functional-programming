Define a `Show`-instance (based on `showsPrec`) for the following enhanced `Expr` data type.
Try to use as less parentheses as possible and document your development with examples.

> data ShowExpr = Binary BinOp ShowExpr ShowExpr
>               | Function Fun ShowExpr
>               | Number Double
>
> data BinOp = Plus | Div
>
> data Fun = SquareRoot

> instance Show ShowExpr where
>   showsPrec _ (Number x) = shows x
>   showsPrec _ (Function f exp) = 
>             showString "SQR" . showChar ' ' . 
>             showParen (case exp of
>                Number x -> False
>                Function _ _ -> True
>                Binary _ _ _ -> True) (shows exp)
>   showsPrec _ (Binary Plus exp1 exp2) =
>           let eval exp = (case exp of
>                Number x -> False
>                Function _ _ -> False
>                Binary _ _ _ -> True)
>           in showParen (eval exp1) (shows exp1) .
>              showChar ' ' . showChar '+' . showChar ' ' .
>              showParen (eval exp2) (shows exp2)
>   showsPrec _ (Binary Div exp1 exp2) =
>           let eval exp = (case exp of
>                Number x -> False
>                Function _ _ -> False
>                Binary _ _ _ -> True)
>           in showParen (eval exp1) (shows exp1) .
>              showChar ' ' . showChar '/' . showChar ' ' .
>              showParen (eval exp2) (shows exp2)
