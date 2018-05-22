> import Free

In class we discussed that we can transform the `Delay` monad into `Free Identity` and vice versa as well as the `Maybe` monad into `Free One`.

1. Give an implementation for the instance `Iso (Delay a) (Free Identity a)`.
2. Give an implementation for the instance `Iso (Maybe a) (Free One a)`.
3. Prove that the round-trip property for `to` and `from` hold for both instances!
