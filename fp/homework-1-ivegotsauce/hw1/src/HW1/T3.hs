module HW1.T3
  ( Tree(..)
  , Meta(..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  , mkBranch
  ) where

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a) deriving Show

data Meta = Meta
  { size   :: Int
  , height :: Int
  } deriving Show

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize (Branch meta _ _ _) = size meta
tsize Leaf                = 0

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth = theight

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ t1 b t2)
  | a == b     = True
  | a < b      = tmember a t1
  | otherwise  = tmember a t2

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a t@(Branch _ t1 b t2)
  | a == b    = t
  | a < b     = balanceTree $ mkBranch (tinsert a t1) b t2
  | otherwise = balanceTree $ mkBranch t1 b (tinsert a t2)

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList []     = Leaf
tFromList (x:xs) = tinsert x $ tFromList xs

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l a r = Branch (Meta (1 + tsize l + tsize r) (1 + max (theight l) (theight r))) l a r

theight :: Tree a -> Int
theight Leaf                = 0
theight (Branch meta _ _ _) = height meta

tleft :: Tree a -> Tree a
tleft Leaf             = Leaf
tleft (Branch _ l _ _) = l

tright :: Tree a -> Tree a
tright Leaf             = Leaf
tright (Branch _ _ _ r) = r

tvalue :: Tree a -> a
tvalue Leaf             = undefined
tvalue (Branch _ _ a _) = a

rotateRight :: Tree a -> Tree a
rotateRight Leaf               = Leaf
rotateRight (Branch _ l val r) = mkBranch (tleft l) (tvalue l) (mkBranch (tright l) val r)

rotateLeft :: Tree a -> Tree a
rotateLeft Leaf               = Leaf
rotateLeft (Branch _ l val r) = mkBranch (mkBranch l val (tleft r)) (tvalue r) (tright r)

balanceFactor :: Tree a -> Int
balanceFactor p = (theight $ tright p) - (theight $ tleft p)

balanceTree :: Tree a -> Tree a
balanceTree p@(Branch _ l a r) = case balanceFactor p of
  2  -> rotateLeft (if balanceFactor r < 0 then mkBranch l a $ rotateRight r else p)
  -2 -> rotateRight (if balanceFactor l > 0 then mkBranch (rotateLeft l) a r else p)
  _  -> p
balanceTree Leaf = Leaf
