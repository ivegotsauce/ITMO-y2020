module HW2.T2
  ( concatLists
  , distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

import HW2.T1

distOption      :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _                = None

distPair        :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

distQuad        :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

distAnnotated   :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# ea, b :# eb) = (a, b) :# (ea <> eb)

distExcept      :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, High b)     = High (a, b)

distStream      :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> as, b :> bs) = (a, b) :> distStream (as, bs)

distList        :: (List a, List b) -> List (a, b)
distList (_, Nil) = Nil
distList (Nil, _) = Nil
distList (a :. as, b) = helper a b `concatLists` distList (as, b)
  where
    helper c (d :. ds) = (c, d) :. helper c ds
    helper _ Nil       = Nil

concatLists :: List a -> List a -> List a
concatLists (c :. cs) ds = c :. concatLists cs ds
concatLists Nil ds       = ds

distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F $ \i -> (f i, g i)

wrapOption      :: a -> Option a
wrapOption = Some

wrapPair        :: a -> Pair a
wrapPair a = P a a

wrapQuad        :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept      :: a -> Except e a
wrapExcept = Success

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

wrapStream      :: a -> Stream a
wrapStream a = a :> wrapStream a

wrapList        :: a -> List a
wrapList a = a :. Nil

wrapFun         :: a -> Fun i a
wrapFun = F . const