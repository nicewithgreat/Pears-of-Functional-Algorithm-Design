{-

In this kata we will implement five of the most fundamental monads.

Newcomers to Haskell find monads to be one of the most intimidating concepts but on a basic level - they are not too difficult to understand.

A datatype forms a monad if it is possible to complete the following definitions such that the monad laws (described below) hold. There's nothing more to it! For a more intuitive understanding then there are a plethora of tutorials which use (sometimes wild) analogies to explain this concept.

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
Monad laws

return x >>= f = f x
m >>= return = m
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
It turns out that many different types of computation can be encapsulated by monads. For example the Maybe monad encapsulates a computation which can fail and State a computation with mutable state.

The five we will implement here are Identity, Maybe, State, Writer and Reader.

Hint: https://www.haskell.org/haskellwiki/Monad_tutorials_timeline

Note: Please feel free to contribute!

FUNDAMENTALSMONADSDATA STRUCTURESFUNCTIONAL PROGRAMMING

-}
{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

data Identity a = Identity a
  deriving (Show, Eq)

data Maybe a = Nothing | Just a
  deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (w, a)}

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v

instance Monad Maybe where
  return = Just
  Nothing >>= f = Nothing
  (Just v) >>= f = f v

instance Monad (State s) where
  return x = State $ \s->(x,s)
  (State g) >>= f = State $ \s ->
                        case g s of
                            (x,s') -> runState (f x) s'

instance Monad (Reader s) where
  return a = Reader $ const a
  (Reader g) >>= f = Reader $ \s ->
                        case g s of 
                            a -> runReader (f a) s

instance Monoid w => Monad (Writer w) where
  return w = Writer (mempty,w)
  (Writer (s, v)) >>= f = case f v of 
                            Writer (s',v') -> Writer (s `mappend` s',v')
