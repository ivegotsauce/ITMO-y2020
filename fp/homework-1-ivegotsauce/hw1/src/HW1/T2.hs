module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , ncmp
  , nFromNatural
  , nToNum
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Data.Maybe
import Numeric.Natural

data N = Z | S N deriving Show

nplus :: N -> N -> N        -- addition
nplus Z a         = a
nplus a Z         = a
nplus (S a) (S b) = S $ S $ nplus a b

nmult :: N -> N -> N        -- multiplication
nmult Z _     = Z
nmult _ Z     = Z
nmult a (S Z) = a
nmult a (S b) = nplus a (nmult a b)

nsub :: N -> N -> Maybe N   -- subtraction     (Nothing if result is negative)
nsub a Z         = Just a
nsub Z _         = Nothing
nsub (S a) (S b) = nsub a b

ncmp :: N -> N -> Ordering  -- comparison
ncmp a b = case nsub a b of
             Nothing -> LT
             Just Z  -> EQ
             _       -> GT

nFromNatural :: Natural -> N
nFromNatural 0   = Z
nFromNatural nat = S $ nFromNatural $ nat - 1

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S a) = 1 + nToNum a

nEven, nOdd :: N -> Bool    -- parity checking
nEven Z     = True
nEven (S a) = not $ nEven a
nOdd = not . nEven

ndiv :: N -> N -> N         -- integer division
ndiv _ Z = error "Division by zero"
ndiv a b
  | a `ncmp` b == LT = Z
  | otherwise        = S $ ndiv (fromJust $ nsub a b) b

nmod :: N -> N -> N         -- modulo operation
nmod a b = fromJust $ nsub a $ nmult (ndiv a b) b
