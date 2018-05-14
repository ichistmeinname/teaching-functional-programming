module Exercises.Exercise4.InterpreterInnermost where

import Exercises.Exercise4.Interpreter

-- |`reduceIM` makes a step with respect to β-reduction, if possible
reduceIM :: Exp -> Maybe Exp
reduceIM (Var _)        = Nothing
reduceIM ((v:->:e):@:f) = case reduceIM f of
  Nothing -> Just (subst v f e)   -- substituiere v durch f in e
  Just f' -> Just ((v:->:e):@:f') -- reduziere f
reduceIM (e1:@:e2)      = case reduceIM e1 of
   -- fmap :: (a -> b) -> Maybe a -> Maybe b, Maybe ist Funktor!
  Nothing  -> (e1:@:) `fmap` reduceIM e2
  Just e1' -> Just (e1':@:e2)
reduceIM (x:->:e) = (x:->:) `fmap` reduceIM e

-- evaluation of an expression as long as possible
evalIM :: Exp -> Exp
evalIM e = maybe e evalIM (reduceIM e)

-- visualisation of evaluation
printStepsIM :: Exp -> IO ()
printStepsIM = mapM_ print . evalSeqIM

-- tracing of all reduction steps
evalSeqIM :: Exp -> [Exp]
evalSeqIM e = e : maybe [] evalSeqIM (reduceIM e)
