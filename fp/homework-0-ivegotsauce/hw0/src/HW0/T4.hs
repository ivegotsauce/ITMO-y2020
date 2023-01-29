{-# LANGUAGE LambdaCase #-}

module HW0.T4
  ( repeat'
  , map'
  , fib
  , fac
  ) where

import Data.Function
import Numeric.Natural

repeat' :: a -> [a]             -- behaves like Data.List.repeat
repeat' x = fix (x:)

map' :: (a -> b) -> [a] -> [b]  -- behaves like Data.List.map
map' = fix (\rec f -> \case
                         (x:xs) -> f x : rec f xs
                         _      -> []
           )

fib :: Natural -> Natural       -- computes the n-th Fibonacci number
fib = fix (\rec curr next n -> if n > 0 then rec next (curr + next) (n - 1) else curr) 0 1

fac :: Natural -> Natural       -- computes the factorial
fac = fix (\rec n -> if n > 1 then n * rec (n - 1) else 1)
