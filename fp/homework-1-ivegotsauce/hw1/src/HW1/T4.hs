module HW1.T4
  ( tfoldr
  , treeToList
  ) where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ b Leaf             = b
tfoldr f b (Branch _ l a r) = tfoldr f (f a $ tfoldr f b r) l

treeToList :: Tree a -> [a]    -- output list is sorted
treeToList = tfoldr (:) []
