> import Interpreter

In class we defined `fac` in lambda calculus.
Compute `fac 1` (by hand or using the interpreter) with respect to outermost and innermost reduction.

> fac = undefined

fac = fix (\g -> \y -> if y == 0 then 1 else y * g (y-1))
LO:
fac 1 = fix (\g -> \y -> if y == 0 then 1 else y * g (y-1)) 1
      = \f. (\x. f (xx)) (\x. f (xx)) (\g -> \y -> if y == 0 then 1 else y * g (y-1)) 1
      = (\x. (\g. \y -> if y == 0 then 1 else y * g (y-1)) (xx)) (\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx)) 1
      = (\g. \y. if y == 0 then 1 else y * g (y-1) ((\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx)) (\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx)))) 1
      = (\y. if y == 0 then 1 else y * ((\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx)) (\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx))) (y-1)) 1
      = (1 * ((\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx)) (\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx))) (1-1))
      = (1 * ((\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx)) (\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx))) 0)
      = (1 * ((\g -> \y -> if y == 0 then 1 else y * g (y-1)) ((\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx))(\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx)))) 0)
      = (1 * (\y . if y == 0 then 1 else y * ((\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx))(\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx))) (y-1)) 0)
      = (1 * (if 0 == 0 then 1 ))
      = 1 * 1
      = 1



      = \f. (\x. f (xx)) (\x. f (xx)) (\f -> \x -> if x == 0 then 1 else x * f (x-1)) 1

LI:

fac 1 = fix (\g -> \y -> if y == 0 then 1 else y * g (y-1)) 1
      = \f. (\x. f (xx)) (\x. f (xx)) (\g -> \y -> if y == 0 then 1 else y * g (y-1)) 1
      = (\x. (\g. \y -> if y == 0 then 1 else y * g (y-1)) (xx)) (\x. (\g -> \y -> if y == 0 then 1 else y * g (y-1)) (xx)) 1
      = (\x. (\g. \y -> if y == 0 then 1 else y * g (y-1)) (xx)) (\y. if y == 0 then 1 else y * (xx) (y-1)) 1
      = (\x. (\g. \y -> if y == 0 then 1 else y * g (y-1)) (xx)) (if 1 == 0 then 1 else 1 * (xx) (1-1))
      = (\x. (\g. \y -> if y == 0 then 1 else y * g (y-1)) (xx)) (1 * (xx) (1-1))
      = (\g. \y -> if y == 0 then 1 else y * g (y-1)) ((1 * (xx) (1-1))(1 * (xx) (1-1)))
      = \y. if y == 0 then 1 else y * ((1 * (xx) (1-1))(1 * (xx) (1-1))) (y-1) 
