> import Exercises.Exercise4.Interpreter

1. Define lambda-expressions for addition, multiplication and exponentation for Church numerals. You can test your code with the provided interpreter (Interpreter.hs).

There (at least) two different approaches to define addition using Church Numerals.
The first idea is to compute `m` times the successor of `n`.

    suc = \n.\f.\x.n f (f x)
    add = \m.\n.m suc n
    
> suc = 'n' :->: 'f' :->: 'x' :->: Var 'n' :@: Var 'f' :@: (Var 'f' :@: Var 'x')
> add = 'm' :->: 'n' :->: Var 'm' :@: suc :@: Var 'n'

    ghci> printSteps $ add :@: nat 1 :@: nat 2
    (\m.\n.m (\n.\f.\x.n f (f x)) n) (\f.\x.f x) (\f.\x.f (f x))
    (\n.(\f.\x.f x) (\n.\f.\x.n f (f x)) n) (\f.\x.f (f x))
    (\f.\x.f x) (\n.\f.\x.n f (f x)) (\f.\x.f (f x))
    (\x.(\n.\f.\x.n f (f x)) x) (\f.\x.f (f x))
    (\n.\f.\x.n f (f x)) (\f.\x.f (f x))
    \f.\x.(\f.\x.f (f x)) f (f x)
    \f.\x.(\x.f (f x)) (f x)
    \f.\x.f (f (f x))

The second idea does not use a helper function to compute the successor, but is defined more directly. That is, first we compute `n` and based on that value we compute `m`; here "compute" can be understood as unrolling the given number in order to have `m+n` applications of `f` in the end.

    add = \m.\n.\f.\x.m f (n f x)
    
> add' = 'm' :->: 'n' :->: 'f' :->: 'x' :->: Var 'm' :@: Var 'f' :@: (Var 'n' :@: Var 'f' :@: Var 'x')

    ghci> printSteps $ add' :@: nat 1 :@: nat 2
    (\m.\n.\f.\x.m f (n f x)) (\f.\x.f x) (\f.\x.f (f x))
    (\n.\f.\x.(\f.\x.f x) f (n f x)) (\f.\x.f (f x))
    \f.\x.(\f.\x.f x) f ((\f.\x.f (f x)) f x)
    \f.\x.(\x.f x) ((\f.\x.f (f x)) f x)
    \f.\x.f ((\f.\x.f (f x)) f x)
    \f.\x.f ((\x.f (f x)) x)
    \f.\x.f (f (f x))

We can define multiplication by adding `m` times the value `n` to zero.

    zero = \f.\x.x
    mul  = \m.\n.m (add n) zero
    
> zero = 'f' :->: 'x' :->: Var 'x'
> mul = 'm' :->: 'n' :->: Var 'm' :@: (add :@: Var 'n') :@: zero

    ghci> printSteps $ mul :@: nat 2 :@: nat 3
    (\m.\n.m ((\m.\n.\f.\x.m f (n f x)) n) (\f.\x.x)) (\f.\x.f (f x)) (\f.\x.f (f (f x)))
    (\n.(\f.\x.f (f x)) ((\m.\n.\f.\x.m f (n f x)) n) (\f.\x.x)) (\f.\x.f (f (f x)))
    (\f.\x.f (f x)) ((\m.\n.\f.\x.m f (n f x)) (\f.\x.f (f (f x)))) (\f.\x.x)
    (\x.(\m.\n.\f.\x.m f (n f x)) (\f.\x.f (f (f x))) ((\m.\n.\f.\x.m f (n f x)) (\f.\x.f (f (f x))) x)) (\f.\x.x)
    (\m.\n.\f.\x.m f (n f x)) (\f.\x.f (f (f x))) ((\m.\n.\f.\x.m f (n f x)) (\f.\x.f (f (f x))) (\f.\x.x))
    (\n.\f.\x.(\f.\x.f (f (f x))) f (n f x)) ((\m.\n.\f.\x.m f (n f x)) (\f.\x.f (f (f x))) (\f.\x.x))
    \f.\x.(\f.\x.f (f (f x))) f ((\m.\n.\f.\x.m f (n f x)) (\f.\x.f (f (f x))) (\f.\x.x) f x)
    \f.\x.(\x.f (f (f x))) ((\m.\n.\f.\x.m f (n f x)) (\f.\x.f (f (f x))) (\f.\x.x) f x)
    \f.\x.f (f (f ((\m.\n.\f.\x.m f (n f x)) (\f.\x.f (f (f x))) (\f.\x.x) f x)))
    \f.\x.f (f (f ((\n.\f.\x.(\f.\x.f (f (f x))) f (n f x)) (\f.\x.x) f x)))
    \f.\x.f (f (f ((\f.\x.(\f.\x.f (f (f x))) f ((\f.\x.x) f x)) f x)))
    \f.\x.f (f (f ((\x.(\f.\x.f (f (f x))) f ((\f.\x.x) f x)) x)))
    \f.\x.f (f (f ((\f.\x.f (f (f x))) f ((\f.\x.x) f x))))
    \f.\x.f (f (f ((\x.f (f (f x))) ((\f.\x.x) f x))))
    \f.\x.f (f (f (f (f (f ((\f.\x.x) f x))))))
    \f.\x.f (f (f (f (f (f ((\x.x) x))))))
    \f.\x.f (f (f (f (f (f x)))))

In the more direct style we use the computation `n f` as function that is unrolled `m` times, that, in the end we have `m * n` applications of `f` for any given `x`.

    mul = \m.\n.\f.m (n f)
    
> mul' = 'm' :->: 'n' :->: 'f' :->: Var 'm' :@: (Var 'n' :@: Var 'f')

    ghci> printSteps $ mul' :@: nat 2 :@: nat 3
    (\m.\n.\f.m (n f)) (\f.\x.f (f x)) (\f.\x.f (f (f x)))
    (\n.\f.(\f.\x.f (f x)) (n f)) (\f.\x.f (f (f x)))
    \f.(\f.\x.f (f x)) ((\f.\x.f (f (f x))) f)
    \f.\x.(\f.\x.f (f (f x))) f ((\f.\x.f (f (f x))) f x)
    \f.\x.(\x.f (f (f x))) ((\f.\x.f (f (f x))) f x)
    \f.\x.f (f (f ((\f.\x.f (f (f x))) f x)))
    \f.\x.f (f (f ((\x.f (f (f x))) x)))
    \f.\x.f (f (f (f (f (f x)))))

Exponentiation is just the repetition of multiplication.

    one = \f.\x.f x
    exp = \m.\n.n (mul m) one
    
> one = 'f' :->: 'x' :->: Var 'f' :@: Var 'x'
> exp = 'm' :->: 'n' :->: Var 'n' :@: (mul :@: Var 'm') :@: one

    ghci> printSteps $ exp :@: nat 3 :@: nat 2
    (\m.\n.n ((\m.\n.\f.m (n f)) m) (\f.\x.f x)) (\f.\x.f (f (f x))) (\f.\x.f (f x))
    (\n.n ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x)))) (\f.\x.f x)) (\f.\x.f (f x))
    (\f.\x.f (f x)) ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x)))) (\f.\x.f x)
    (\x.(\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) x)) (\f.\x.f x)
    (\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x))
    (\n.\f.(\f.\x.f (f (f x))) (n f)) ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x))
    \f.(\f.\x.f (f (f x))) ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f)
    \f.\x.(\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))
    \f.\x.(\n.\f.(\f.\x.f (f (f x))) (n f)) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))
    \f.\x.(\f.(\f.\x.f (f (f x))) ((\f.\x.f x) f)) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))
    \f.\x.(\f.\x.f (f (f x))) ((\f.\x.f x) f) ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))
    \f.\x.(\x.(\f.\x.f x) f ((\f.\x.f x) f ((\f.\x.f x) f x))) ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))
    \f.\x.(\a.\x.a x) f ((\a.\x.a x) f ((\a.\x.a x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))
    \f.\x.(\x.f x) ((\a.\x.a x) f ((\a.\x.a x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))
    \f.\x.f ((\a.\x.a x) f ((\a.\x.a x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))
    \f.\x.f ((\x.f x) ((\a.\x.a x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))
    \f.\x.f (f ((\a.\x.a x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))
    \f.\x.f (f ((\x.f x) ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))
    \f.\x.f (f (f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))
    \f.\x.f (f (f ((\n.\f.(\f.\x.f (f (f x))) (n f)) (\f.\x.f x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))
    \f.\x.f (f (f ((\f.(\f.\x.f (f (f x))) ((\f.\x.f x) f)) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))
    \f.\x.f (f (f ((\f.\x.f (f (f x))) ((\f.\x.f x) f) ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))
    \f.\x.f (f (f ((\x.(\f.\x.f x) f ((\f.\x.f x) f ((\f.\x.f x) f x))) ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))
    \f.\x.f (f (f ((\a.\x.a x) f ((\a.\x.a x) f ((\a.\x.a x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))))
    \f.\x.f (f (f ((\x.f x) ((\a.\x.a x) f ((\a.\x.a x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))))
    \f.\x.f (f (f (f ((\a.\x.a x) f ((\a.\x.a x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))))
    \f.\x.f (f (f (f ((\x.f x) ((\a.\x.a x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))))
    \f.\x.f (f (f (f (f ((\a.\x.a x) f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))))
    \f.\x.f (f (f (f (f ((\x.f x) ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))))
    \f.\x.f (f (f (f (f (f ((\m.\n.\f.m (n f)) (\f.\x.f (f (f x))) (\f.\x.f x) f x))))))
    \f.\x.f (f (f (f (f (f ((\n.\f.(\f.\x.f (f (f x))) (n f)) (\f.\x.f x) f x))))))
    \f.\x.f (f (f (f (f (f ((\f.(\f.\x.f (f (f x))) ((\f.\x.f x) f)) f x))))))
    \f.\x.f (f (f (f (f (f ((\f.\x.f (f (f x))) ((\f.\x.f x) f) x))))))
    \f.\x.f (f (f (f (f (f ((\x.(\f.\x.f x) f ((\f.\x.f x) f ((\f.\x.f x) f x))) x))))))
    \f.\x.f (f (f (f (f (f ((\f.\x.f x) f ((\f.\x.f x) f ((\f.\x.f x) f x))))))))
    \f.\x.f (f (f (f (f (f ((\x.f x) ((\f.\x.f x) f ((\f.\x.f x) f x))))))))
    \f.\x.f (f (f (f (f (f (f ((\f.\x.f x) f ((\f.\x.f x) f x))))))))
    \f.\x.f (f (f (f (f (f (f ((\x.f x) ((\f.\x.f x) f x))))))))
    \f.\x.f (f (f (f (f (f (f (f ((\f.\x.f x) f x))))))))
    \f.\x.f (f (f (f (f (f (f (f ((\x.f x) x))))))))
    \f.\x.f (f (f (f (f (f (f (f (f x))))))))

We can also define it in direct style by using `m` as function `n` is applied to, that is, `m` is unrolled `n` times.

    exp = \m.\n.n m
    
> exp' = 'm' :->: 'n' :->: Var 'n' :@: Var 'm'

    ghci> printSteps $ exp' :@: nat 3 :@: nat 2
    (\m.\n.n m) (\f.\x.f (f (f x))) (\f.\x.f (f x))
    (\n.n (\f.\x.f (f (f x)))) (\f.\x.f (f x))
    (\f.\x.f (f x)) (\f.\x.f (f (f x)))
    \x.(\f.\x.f (f (f x))) ((\f.\x.f (f (f x))) x)
    \x.\a.(\f.\x.f (f (f x))) x ((\f.\x.f (f (f x))) x ((\f.\x.f (f (f x))) x a))
    \x.\a.(\a.x (x (x a))) ((\f.\x.f (f (f x))) x ((\f.\x.f (f (f x))) x a))
    \x.\a.x (x (x ((\f.\x.f (f (f x))) x ((\f.\x.f (f (f x))) x a))))
    \x.\a.x (x (x ((\a.x (x (x a))) ((\f.\x.f (f (f x))) x a))))
    \x.\a.x (x (x (x (x (x ((\f.\x.f (f (f x))) x a))))))
    \x.\a.x (x (x (x (x (x ((\a.x (x (x a))) a))))))
    \x.\a.x (x (x (x (x (x (x (x (x a))))))))


2. The computation of `pred 2` where pred is defined as follows

    pred = \n.\f.\x.n (\g.\h.h (g f)) (\u.x) (\u.u)

is something that we want to deligate to the interpreter.

> pred = 'n' :->: 'f' :->: 'x' :->: Var 'n' :@: ('g' :->: 'h' :->: Var 'h' :@: (Var 'g' :@: Var 'f')) :@: ('u' :->: Var 'x') :@: ('u' :->: Var 'u')

    ghci> printSteps $ pred :@: nat 2
    (\n.\f.\x.n (\g.\h.h (g f)) (\u.x) (\u.u)) (\f.\x.f (f x))
    \f.\x.(\f.\x.f (f x)) (\g.\h.h (g f)) (\u.x) (\u.u)
    \f.\x.(\x.(\g.\h.h (g f)) ((\g.\h.h (g f)) x)) (\u.x) (\u.u)
    \f.\x.(\g.\h.h (g f)) ((\g.\h.h (g f)) (\u.x)) (\u.u)
    \f.\x.(\h.h ((\g.\h.h (g f)) (\u.x) f)) (\u.u)
    \f.\x.(\u.u) ((\g.\h.h (g f)) (\u.x) f)
    \f.\x.(\g.\h.h (g f)) (\u.x) f
    \f.\x.(\h.h ((\u.x) f)) f
    \f.\x.f ((\u.x) f)
    \f.\x.f x

There's an alternative version of `pred` using booleans and pairs. The key idea is to have a pair of `(n,succ n)` at each step and yield the first component when the computation is done.

    true  = \t.\f.t
    false = \t.\f.f
    
    pair  = \x.\y.\f.f x y
    fst   = \p.p true
    snd   = \p.p false
    
    pred  = \n.fst (n (\p.pair (snd p) (suc (snd p))) (pair zero zero))
    
> true' = 't' :->: 'f' :->: Var 't'
> false' = 't' :->: 'f' :->: Var 'f'
> pair = 'x' :->: 'y' :->: 'f' :->: Var 'f' :@: Var 'x' :@: Var 'y'
> fst' = 'p' :->: Var 'p' :@: true'
> snd' = 'p' :->: Var 'p' :@: false'
> pred' = 'n' :->: fst' :@: (Var 'n' :@: ('p' :->: pair :@: (snd' :@: Var 'p') :@: (suc :@: (snd' :@: Var 'p'))) :@: (pair :@: zero :@: zero))

    ghci> printSteps $ pred' :@: nat 2
    (\n.(\p.p (\t.\f.t)) (n (\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))) (\f.\x.f (f x))
    (\p.p (\t.\f.t)) ((\f.\x.f (f x)) (\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))
    (\f.\x.f (f x)) (\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)) (\t.\f.t)
    (\x.(\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) x)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)) (\t.\f.t)
    (\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x))) (\t.\f.t)
    (\x.\y.\f.f x y) ((\p.p (\t.\f.f)) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x))))) (\t.\f.t)
    (\y.\f.f ((\p.p (\t.\f.f)) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))) y) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x))))) (\t.\f.t)
    (\f.f ((\p.p (\t.\f.f)) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))))) (\t.\f.t)
    (\t.\f.t) ((\p.p (\t.\f.f)) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))))
    (\f.(\p.p (\t.\f.f)) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))))
    (\p.p (\t.\f.f)) ((\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))
    (\p.(\x.\y.\f.f x y) ((\p.p (\t.\f.f)) p) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) p))) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)) (\t.\f.f)
    (\x.\y.\f.f x y) ((\p.p (\t.\f.f)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x))) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))) (\t.\f.f)
    (\y.\f.f ((\p.p (\t.\f.f)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x))) y) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))) (\t.\f.f)
    (\f.f ((\p.p (\t.\f.f)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x))) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x))))) (\t.\f.f)
    (\t.\f.f) ((\p.p (\t.\f.f)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x))) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x))))
    (\f.f) ((\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x))))
    (\n.\f.\x.n f (f x)) ((\p.p (\t.\f.f)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)))
    \f.\x.(\p.p (\t.\f.f)) ((\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x)) f (f x)
    \f.\x.(\x.\y.\f.f x y) (\f.\x.x) (\f.\x.x) (\t.\f.f) f (f x)
    \f.\x.(\y.\f.f (\f.\x.x) y) (\f.\x.x) (\t.\f.f) f (f x)
    \f.\x.(\f.f (\f.\x.x) (\f.\x.x)) (\t.\f.f) f (f x)
    \f.\x.(\t.\f.f) (\f.\x.x) (\f.\x.x) f (f x)
    \f.\x.(\f.f) (\f.\x.x) f (f x)
    \f.\x.(\f.\x.x) f (f x)
    \f.\x.(\x.x) (f x)
    \f.\x.f x



 3. Prove the theorem about fixpoints that was introduced in class. That is, you have to show that for every lamba expression `F` it holds that `Y F = F (Y F)` where `Y` is the `Y`-combinator.

 We can prove the fix-point theorem by reasoning as follows (where `=` is used as equivalence relation with beta-reduction).
Recall the definition of the Y-combinator.

    Y = \f.(\x.f (x x)) (\x.f (x x))

And then reason as follows.

      Y F
    = (\f.(\x.f (x x)) (\x.f (x x))) F
    = (\x.F (x x)) (\x.F (x x))
    = F ((\x.F (x x)) (\x.F (x x)))
    = F ((\f.(\x.f (x x)) (\x.f (x x))) F)
    = F (Y F)

That is, for every lambda-expression `F` there exists a lambda-expression `P`, such that `F P = P` holds, namely, `P = Y F`.
