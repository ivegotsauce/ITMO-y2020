module HW0.T5
  ( Nat
  , nz
  , ns
  , nplus
  , nmult
  , nFromNatural
  , nToNum
  ) where

import Numeric.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz f x = x

ns :: Nat a -> Nat a
ns f g x = f g $ g x

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus f g h x = g h $ f h x
nmult f g = f . g

nFromNatural :: Natural -> Nat a
nFromNatural n = if n > 0 then ns $ nFromNatural (n - 1) else nz

nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0
