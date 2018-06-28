One "fun fact" about algebraic data structures is that you can derive the number of values of
a given type when using its basic representation via products and sums.
First, consider the constants for 0, 1 and 2.

> -- Void has no value
> data Void
>
> -- () has 1 value
> -- data () = ()
>
> -- Bool has 2 values
> -- data Bool = True | False

We can derive the number of values using "basic arithmetics".
A product type is represented using `(,)`, a sum type using `Either`.

> -- 2 * 1 = 2
> type TwoTimesOne = (Bool,())
> tto1 = (True, ())
> tto2 = (False, ())

> -- 2 + 1 = 3
> type TwoPlusOne = Either Bool ()
> tpo1 = Left True
> tpo2 = Left False
> tpo3 = Right ()


The basic idea of this fun fact is that data types in Haskell can be represented using
sum and product types only.
For every data type there is an isomorphic representation using `(,)` and `Either` (and recursion).

The essence of lenses and prisms is that we have two functions, which form an isomorphism,
that transforms a given data structure into its alternative representation and vice versa.

Give implementations for the following lenses and prisms and state which "arithmetic laws"
hide behind the isomorphic functions you need to provide.
That is, think about the hidden type `q` that is internally used when defining a lens/prism.

(1) `Lens a ()`
(2) `Lens (Either a a) a`
(3) `Lens (Bool -> a) a`

(4) `Prism (Bool,a) a`
(5) `Prism a a`
(6) `Prism a Void`

Based on this idea, define lenses/prisms for the following functions.

(7) `nil` --- checks if a list is empty
(8) `isJust`  --- checks if the given optional value is present
(9) `tail` --- checks if the given list is non-empty and yields the tail if available

Hint: Sometimes you have to give more than one implementation.
