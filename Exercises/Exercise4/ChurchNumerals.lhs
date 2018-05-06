> import Interpreter
> import Prelude hiding (pred)

1. Define lambda-expressions for addition, multiplication and exponentation for Church numerals. You can test your code with the provided interpreter (Interpreter.hs).
 
> add = z :->: y :->: f :->: x :->: Var z :@: Var f :@: (Var y :@: Var f :@: Var x)
> mult = z :->: y :->: f :->: Var y :@: (Var z :@: Var f)
> power = z :->: y :->: Var z :@: Var y    


2. The predecessor of a Chuch numeral can be defined as follows.

 pred = \n.\f.\x.n (\g.\h.h (g f)) (\u.x) (\u.u)
 Compute `pred 2` by hand.
 2 = \f.\x. f (f x)
 
 pred 2 = (\n.\f.\x.n (\g.\h.h (g f)) (\u.x) (\u.u))  (2)
        = (\n.\f.\x.n (\g.\h.h (g f)) (\u.x) (\u.u))  (\f.\x. f (f x))
        = (\f.\x. (\f.\x. f (f x)) (\g.\h.h (g f)) (\u.x) (\u.u))  
        = (\f.\x. (\x. (\g.\h.h (g f)) ((\g.\h.h (g f)) x)) (\u.x) (\u.u))  
        = (\f.\x. ((\g.\h.h (g f)) ((\h.h ((\u.x) f)))) (\u.u)) 
        = (\f.\x. ((\g.\h.h (g f)) (\h.h x) (\u.u))
        = (\f.\x. ((\h.h ((\h.h x) f)) (\u.u))
        = (\f.\x. ((\h.h (f x)) (\u.u))
        = (\f.\x. ((\u.u) (f x)))
        = (\f.\x. (f x))         
        = \f.\x. f x

3. Prove the theorem about fixpoints that was introduced in class. That is, you have to show that for every lamba expression `F` it holds that `Y F = F (Y F)` where `Y` is the `Y`-combinator.

Y F = (λf. (λx.f(x x)) (λx.f(x x))) F
    = ((λx.F(x x)) (λx.F(x x)))
    = (F((λx.F(x x)) (λx.F(x x))))
    = (F((λf. (λx.f(x x)) (λx.f(x x))) F))
    = F (Y F)    

> pred = z :->: f :->: x :->: Var z :@: (g :->: h :->: Var h :@: (Var g :@: Var f)) :@: (y :->: Var x) :@: (y :->: Var y) 
