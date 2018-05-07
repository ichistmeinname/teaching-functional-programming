module Interpreter where

infixl 4 :@:  -- Applikation ist linksassoziativ und bindet stärker
infixr 3 :->: -- Abstraktion rechtsassoziativ

-- |Variablennamen
type Name = Char

-- |Lambda Ausdrücke stellen wir mit dem folgenden Datentyp dar.
--   Wir verwenden Infix-Konstruktoren zur besseren Lesbarkeit.
data Exp
  = Var Name      -- Variable
  | Exp  :@:  Exp -- Applikation
  | Name :->: Exp -- Abstraktion
 deriving Eq

-- `reduce` führt einen Outermost-Schritt entsprechend der β-Reduktion aus,
--  falls das möglich ist.
reduce :: Exp -> Maybe Exp
reduce (Var _)        = Nothing
reduce ((v:->:e):@:f) = Just (subst v f e) -- substituiere v durch f in e
reduce (e1:@:e2)      = case reduce e1 of
   -- fmap :: (a -> b) -> Maybe a -> Maybe b, Maybe ist Funktor!
  Nothing  -> (e1:@:) `fmap` reduce e2
  Just e1' -> Just (e1':@:e2)
reduce (x:->:e) = (x:->:) `fmap` reduce e

-- `subst` implementiert die Substitution aus der Vorlesung
subst :: Name -> Exp -> Exp -> Exp
subst v f (Var x)   = if x == v then f else Var x
subst v f (e1:@:e2) = subst v f e1 :@: subst v f e2
subst v f (x:->:e)  | v == x          = v :->: e
                    | x `elem` free f = y :->: subst v f (subst x (Var y) e)
                    | otherwise       = x :->: subst v f e
                    where y:_ = filter (`notElem` v : free e ++ free f) ['a'..]

-- | Berechnung der freien Variablen
free :: Exp -> [Char]
free (Var v)   = [v]
free (e1:@:e2) = free e1 ++ free e2
free (v:->:e)  = filter (/=v) (free e)

-- Die Auswertungsfunktion verwendet die vordefinierte Funktion `maybe`
--  zur Fallunterscheidung über Maybe-Werte.
eval :: Exp -> Exp
eval e = maybe e eval (reduce e)

-- Zum Testen, hier eine Funktion, die eine Ableitung ausgibt
printSteps :: Exp -> IO ()
printSteps = mapM_ print . evalSeq

evalSeq :: Exp -> [Exp]
evalSeq e = e : maybe [] evalSeq (reduce e)

instance Show Exp where
  showsPrec _ (Var n)   = (n:)
  showsPrec p (e1:@:e2) = br (p>2) (showsPrec 2 e1 . (' ':) . showsPrec 3 e2)
  showsPrec p (n:->:e)  = br (p>1) (('\\':) . (n:) . ('.':) . showsPrec 1 e)

br :: Bool -> ShowS -> ShowS
br b s = if b then ('(':) . s . (')':) else s

-- Es folgen ein paar Beispiele
f = 'f'
g = 'g'
h = 'h'
x = 'x'
y = 'y'
z = 'z'

fixE = f :->: (x :->: Var f :@: (Var x :@: Var x))
              :@: (x :->: Var f :@: (Var x :@: Var x))

-- Boole'sche Werte (Church-kodiert)
true  = x :->: (y :->: Var x)
false = x :->: (y :->: Var y)

-- if-then-else
cond = f :->: x :->: y :->: Var f :@: Var x :@: Var y

-- Abkuerzung zum Erzeugen von Zahlen
nat :: Int -> Exp
nat n = f :->: x :->: foldr (\_ -> (Var f :@:)) (Var x) [1..n]
