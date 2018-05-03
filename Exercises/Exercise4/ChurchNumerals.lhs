 1. Define lambda-expressions for addition, multiplication and exponentation for Church numerals. You can test your code with the provided interpreter (Interpreter.hs).

 2. The predecessor of a Chuch numeral can be defined as follows.

   pred = \n.\f.\x.n (\g.\h.h (g f)) (\u.x) (\u.u)

  Compute `pred 2` by hand.

 3. Prove the theorem about fixpoints that was introduced in class. That is, you have to show that for every lamba expression `F` it holds that `Y F = F (Y F)` where `Y` is the `Y`-combinator.

> pred = undefined
