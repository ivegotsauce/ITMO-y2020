module HW1.T6
  ( mcat
  , epart
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap mcatImpl
  where
    mcatImpl (Just x) = x
    mcatImpl _        = mempty

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap epartImpl
  where
    epartImpl (Left a)  = (a, mempty)
    epartImpl (Right b) = (mempty, b)
