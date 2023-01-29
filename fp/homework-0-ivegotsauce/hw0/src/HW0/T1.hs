{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->) (Iso)
  , flipIso
  , runIso
  , distrib
  , assocPair
  , assocEither
  ) where

data a <-> b = Iso ( a -> b ) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a)       = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

necessity :: (a, (b, c)) -> ((a, b), c)
necessity (a, (b, c)) = ((a, b), c)

sufficiency :: ((a, b), c) -> (a, (b, c))
sufficiency ((a, b), c) = (a, (b, c))

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso necessity sufficiency

eitherNecessity :: Either a (Either b c) -> Either (Either a b) c
eitherNecessity (Left a)          = Left (Left a)
eitherNecessity (Right (Left b))  = Left (Right b)
eitherNecessity (Right (Right c)) = Right c

eitherSufficiency :: Either (Either a b) c -> Either a (Either b c)
eitherSufficiency (Left (Left a))  = Left a
eitherSufficiency (Left (Right b)) = Right (Left b)
eitherSufficiency (Right c)        = Right (Right c)

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso eitherNecessity eitherSufficiency
