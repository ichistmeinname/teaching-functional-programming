{-# LANGUAGE RankNTypes #-}

module CMonoid where

import Control.Monad
import Data.Monoid

newtype C m a = C ((a -> m) -> m)

instance Monoid m => Monoid (C m a) where
  mempty              = C (\_ -> mempty)
  C ca `mappend` C cb = C (\k -> ca k `mappend` cb k)

instance Functor (C m) where
  fmap f fx = fx >>= return . f

instance Applicative (C m) where
  pure = return
  (<*>) = ap

instance Monad (C m) where
  return x = C ($x)

  C ac >>= f = C (\k -> ac (\x -> let C cb = f x in cb k))

fromC :: Monoid m => (a -> m) -> C m a -> m
fromC f (C ca) = ca f

toC :: Monoid m => [a] -> C m a
toC = mconcat . map return

{-
newtype All = All { getAll :: Bool }

instance Monoid All where
  mempty = All True

  x `mappend` y = All (getAll x && getAll y)
-}

nullC :: (forall m . Monoid m => C m a) -> Bool
nullC l = getAll (fromC (\_ -> All False) l)

{-
newtype Sum a = Sum { getSum :: a }

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

  x `mappend` y = Sum (getSum x + getSum y)
-}

lengthC :: (forall m . Monoid m => C m a) -> Int
lengthC l = getSum (fromC (\_ -> Sum 1) l)

nullLength :: (forall m . Monoid m => C m a) -> (Bool,Int)
nullLength l = (nullC l, lengthC l)

newtype List a = List (forall m . Monoid m => (a -> m) -> m)

instance Monoid (List a) where
  mempty = List (\_ -> mempty)

  List ca `mappend` List cb =
    List (\k -> ca k `mappend` cb k)

instance Functor List where
  fmap f fx = fx >>= return . f

instance Applicative List where
  pure = return
  (<*>) = ap

instance Monad List where
  return x      = List ($x)
  List ca >>= f = ca f
    -- List (\k -> ca (\x -> let List cb = f x in cb k))

hom :: Monoid m => (a -> m) -> List a -> m
hom f (List ca) = ca f

toList :: List a -> [a]
toList = hom (:[])

fromList :: [a] -> List a
fromList = mconcat . map return

nullL :: List a -> Bool
nullL = getAll . hom (\_ -> All False)

lengthL :: List a -> Int
lengthL = getSum . hom (\_ -> Sum 1)

foo :: List a -> (Bool,Int)
foo cm = (nullL cm, lengthL cm)
