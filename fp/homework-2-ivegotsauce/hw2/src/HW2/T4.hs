module HW2.T4
  ( Expr (..)
  , Prim (..)
  , State (..)
  , eval
  , evalImpl
  , joinState
  , mapState
  , modifyState
  , wrapState
  ) where

import Control.Monad (ap)
import HW2.T1

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S $ mapAnnotated f . g

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState (S f) = S $ \s -> case (f s) of
                              (st :# ss) ->  runS st ss

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure    = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y         = Op (Add x y)
  x * y         = Op (Mul x y)
  x - y         = Op (Sub x y)
  abs x         = Op (Abs x)
  signum x      = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y          = Op (Div x y)
  fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval e = evalImpl e modifyState eval

evalImpl
  :: Monad m
  => Expr
  -> (([Prim Double] -> [Prim Double]) -> m a)
  -> (Expr -> m Double)
  -> m Double
evalImpl (Val v) _ _          = return v
evalImpl (Op (Add x y)) mf ef = evalBinary x y Add (+) mf ef
evalImpl (Op (Mul x y)) mf ef = evalBinary x y Mul (*) mf ef
evalImpl (Op (Sub x y)) mf ef = evalBinary x y Sub (-) mf ef
evalImpl (Op (Div x y)) mf ef = evalBinary x y Div (/) mf ef
evalImpl (Op (Abs x)) mf ef   = evalUnary x Abs abs mf ef
evalImpl (Op (Sgn x)) mf ef   = evalUnary x Sgn signum mf ef

evalBinary
  :: Monad m
  => Expr
  -> Expr
  -> (Double -> Double -> Prim Double)
  -> (Double -> Double -> Double)
  -> (([Prim Double] -> [Prim Double]) -> m a)
  -> (Expr -> m Double)
  -> m Double
evalBinary l r c op mf ef = do
  a <- ef l
  b <- ef r
  _ <- mf ((c a b) :)
  pure $ op a b

evalUnary
  :: Monad m
  => Expr
  -> (Double -> Prim Double)
  -> (Double -> Double)
  -> (([Prim Double] -> [Prim Double]) -> m a)
  -> (Expr -> m Double)
  -> m Double
evalUnary e c op mf ef = do
  a <- ef e
  _ <- mf ((c a) :)
  pure $ op a
