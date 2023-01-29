module HW2.T5
  ( EvaluationError (..)
  , ExceptState (..)
  , eval
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , throwExceptState
  , wrapExceptState
  ) where

import Control.Monad (ap, when)
import HW2.T1
import HW2.T4 hiding (eval)

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) = ES $ mapExcept (mapAnnotated f) . g

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES $ \s -> case f s of
                                      Error e            -> Error e
                                      Success (es :# ss) -> runES es ss

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure    = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Op (Div x y)) = do
  a <- eval x
  b <- eval y
  when (b == 0) $ throwExceptState DivideByZero
  modifyExceptState (Div a b :)
  pure $ a / b
eval e = evalImpl e modifyExceptState eval
