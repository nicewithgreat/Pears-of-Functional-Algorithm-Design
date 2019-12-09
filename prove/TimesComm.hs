{-
Successor of A+B=B+A and A+(B+C)=(A+B)+C (please do these two first, the solutions are required doing this Kata), but now you'll need to prove A*B=B*A. There'll be a lot of boilerplate code.

FUNDAMENTALSNUMBERSMATHEMATICSALGORITHMS
-}
{--simple test


-- {-# LANGUAGE GADTs           #-}
-- {-# LANGUAGE TypeFamilies    #-}
-- {-# LANGUAGE TypeOperators   #-}
-- {-# LANGUAGE TemplateHaskell #-}

-- module Kata.TimesComm.TestSpec (main) where

-- import Kata.TimesComm
-- import Kata.TimesComm.Definitions

-- import Test.Hspec

-- -- | Verify that the functions' signature is correct:
-- solution :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
-- solution = timesComm

-- main :: IO ()
-- main = hspec $ do
--   describe "Proof checking" $ do
--     it "Doesn't use any unsafe modules" $
--       hidden [Module "Unsafe.Coerce"]
--}

{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

-- module Kata.TimesComm where

-- import Kata.TimesComm.Definitions

--Preloaded code. Maybe helpful for local editing.

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | Peano definition of multiplication.
type family (:*:) (n :: *) (m :: *) :: *
type instance Z :*: m = Z
type instance S n :*: m = m :+: (n :*: m)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS n) = EqlS $ symmetric n

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- This will be helpful
plus' :: Equal n m -> Natural a -> Equal (n :+: a) (m :+: a)
plus' EqlZ a = reflexive a
plus' (EqlS enm) a = EqlS $ plus' enm a

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-plus-c-equals-a-plus-b-plus-c-prove-it/haskell
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ NumZ NumZ = EqlZ
plusAssoc NumZ NumZ (NumS q) = EqlS $ plusAssoc NumZ NumZ q
plusAssoc NumZ (NumS q) w = EqlS $ plusAssoc NumZ q w
plusAssoc (NumS q) w e = EqlS $ plusAssoc q w e

-- | if a = b and b = c, then a = c.
(<=>) :: Equal a b -> Equal b c -> Equal a c
EqlZ <=> EqlZ = EqlZ
(EqlS ab) <=> (EqlS bc) = EqlS (ab <=> bc)

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/haskell
plusComm :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusComm NumZ NumZ = EqlZ
plusComm NumZ (NumS b) = EqlS (plusComm NumZ b)
plusComm (NumS a) NumZ = EqlS (plusComm a NumZ)
plusComm sa@(NumS a) sb@(NumS b) = EqlS ((plusComm a sb) <=> (EqlS (plusComm b a)) <=> (plusComm sa b))

-- -- | Peano definition of multiplication.
-- type family (:*:) (n :: *) (m :: *) :: *
-- type instance Z :*: m = Z
-- type instance S n :*: m = m :+: (n :*: m)

-- This will also be helpful
zeroComm :: Natural a -> Equal Z (a :*: Z)
zeroComm NumZ = EqlZ
zeroComm (NumS a) = zeroComm a
-- plus' :: Equal n m -> Natural a -> Equal (n :+: a) (m :+: a)
-- -- This is the proof that the kata requires.
-- -- | a * b = b * a
timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm NumZ b = zeroComm b
timesComm a NumZ = symmetric $ zeroComm a
timesComm na@(NumS a) nb@(NumS b) = EqlS $ (plus' (timesComm a nb) b) <=> (plus' (plus' (timesComm a b)a)b)