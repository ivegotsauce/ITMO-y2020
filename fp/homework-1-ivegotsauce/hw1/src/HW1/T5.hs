module HW1.T5
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty hiding (tail)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn x = foldr splitOnImpl ([] :| [])
  where
    splitOnImpl z acc@(y :| ys)
      | x == z    = [] <| acc
      | otherwise = (z : y) :| ys

joinWith :: a -> NonEmpty [a] -> [a]
joinWith x = tail . (foldr joinWithImpl [])
  where
    joinWithImpl z acc = (x : z) ++ acc
