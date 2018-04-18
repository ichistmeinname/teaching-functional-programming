Let's take a look at several modest looking data types and their corresponding functor instances.

We need this extension later.

> {-# LANGUAGE EmptyCase #-}
 
> data Zero a
> data One a = One
> data Identity a = Identity a

We start with the functor instance for |Identity|.
There (at least) two versions to define the instance: via pattern matching on
the left-hand side or via a case-expression on the right-hand side.

> instance Functor Identity where
>     -- fmap f (Identity x) = Identity (f x)
>     fmap f ix = case ix of
>                   Identity x -> Identity (f x)

For the odd-looking data type |One|, we do not even need pattern matching.

> instance Functor One where
>     -- fmap :: (a -> b) -> One a -> One b
>     fmap f _ = One

Note that |One a| is usually named |One| because only has one value.

The same idea applies to the name of |Zero a| that has no values, which seems very odd at first.

> instance Functor Zero where
>    -- fmap f x = undefined

Since there is no value of type |Zero a| it is not clear what to provide on the right-hand side,
it seems that we can only use |undefined|.

However, if we use the language extension mentioned above, there is a better definition that we can use.

>    fmap f x = case x of

Note that the argument |x| that is introduced on the left-hand side is a value of type |Zero a|, but
as we just discussed, there actually is no value of that type. So, the definition basically calls the bluff!
We say, okay, if you give me a value |x| of type |Zero a| then show it to me!
However, there are no constructors to pattern match on, thus, the empty case expression is enough to make
the definition work and compile successfully.
The idea corresponds to the situation in logic where we can follow anything from a false assumption.
Consider the following functions.

> absurd :: Zero a -> b
> absurd x = case x of

Here we also have an implication: if you give me something of type |Zero a|, I can give you something of type |b|.
This definition seems pretty absurd, however, if follows the same idea as above. Once again, we call the bluff
on the supplied argument |x| and, thus, can follow anything from the false assumption.
