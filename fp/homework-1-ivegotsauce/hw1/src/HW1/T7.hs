module HW1.T7
  ( ListPlus(..)
  , Inclusive(..)
  , DotString(..)
  , Fun(..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a deriving Show
infixr 5 :+

instance Semigroup (ListPlus a) where
  Last a <> b   = a :+ b
  (a :+ b) <> c = a :+ b <> c

data Inclusive a b = This a | That b | Both a b deriving Show

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This i <> This j     = This (i <> j)
  This i <> That j     = Both i j
  That i <> This j     = Both j i
  That i <> That j     = That (i <> j)
  This i <> Both j k   = Both (i <> j) k
  That i <> Both j k   = Both j (i <> k)
  Both i j <> This k   = Both (i <> k) j
  Both i j <> That k   = Both i (j <> k)
  Both i j <> Both k p = Both (i <> k) (j <> p)

newtype DotString = DS String deriving Show

instance Semigroup DotString where
  a <> DS ""   = a
  DS "" <> a   = a
  DS a <> DS b = DS (a ++ '.' : b)

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F f <> F g = F (f . g)

instance Monoid (Fun a) where
  mempty = F id
