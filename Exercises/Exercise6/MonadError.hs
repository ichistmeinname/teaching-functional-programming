{-# Language MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Exercises.Exercise6.MonadError where

import Prelude hiding ( catch )
import Control.Monad (ap)

class Monad m => MonadError e m where

    throw :: e -> m a
    catch :: m a -> (e -> m a) -> m a

data Error err res = Thrown err | Result res
    deriving Show

instance Functor (Error err) where
    fmap f (Thrown err) = Thrown err
    fmap f (Result res) = Result (f res)

instance Applicative (Error err) where
    pure = return
    (<*>) = ap

instance Monad (Error err) where

    return = Result

    Thrown e >>= _ = Thrown e
    Result r >>= f = f r

instance MonadError err (Error err) where

    throw = Thrown
    catch (Thrown e) h = h e
    catch (Result r) _ = Result r


data Expr = Expr :+: Expr
          | Expr :/: Expr
          | Sqrt Expr
          | Num Double

data ExprEvalError = DivByZero | NegativeNumber | CustomErr String

instance Show ExprEvalError where

    show DivByZero       = "Division by zero"
    show NegativeNumber  = "Root of a negative number"
    show (CustomErr str) = str

eval :: MonadError ExprEvalError m => Expr -> m Double
eval (Num d) = return d
eval (el :+: er) = do
    nl <- eval el
    nr <- eval er
    return (nl + nr)
eval (el :/: er) = do
    nl <- eval el
    nr <- eval er
    if nr == 0
     then throw DivByZero
     else return (nl / nr)
eval (Sqrt e) = do
    r <- eval e
    if r < 0
     then throw NegativeNumber
     else return (sqrt r)

simpleEval :: Expr -> Error ExprEvalError Double
simpleEval = eval

runError :: Show e => Error e a -> a
runError (Thrown e) = error (show e)
runError (Result x) = x

sixteenToNine :: Expr
sixteenToNine = (Num 5 :+: Num 11) :/: Num 9

divByZero :: Expr
divByZero = (Sqrt (Num 16) :+: Num 8) :/: (Num (-1) :+: Num 1)

sqrtNeg :: Expr
sqrtNeg = Sqrt (Num 3 :+: (Num 5 :/: Num (-1)))
