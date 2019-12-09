{-
Before you begin! This kata is intended as a successor to the kata Even + Odd = Odd? Prove it! If you haven't completed it, you will find this kata very difficult.

What's this kata about?
In this kata, you will prove the commutativity of addition, that is to say, the fact that a + b = b + a. Specifically you will be proving it for the natural numbers.

What can I use?
You have three useful datastructures defined for you in the module Kata.AdditionCommutes.Definitions. You are given no more help in creating this proof.

The natural numbers
This is a very simple definition of the natural numbers, using types.

data Z
data S n
The 'Natural' type
This can be thought of as a predicate, meaning that some number n is a natural number. This is useful for passing numbers to our proofs.

data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)
The 'Equal' type
This is a statement of equality on natural numbers.

data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)
The addition type family
This is Peano's description of addition.

type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)
What's the final goal?
You must write a proof, ie: a function, as so:

plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a) 
This will be tested for cheats, ie: using undefined instead of the actual proof.

What problems might I have?
The challenge of this kata is getting the proof to typecheck. As such, most of the errors produced will be type errors. Cheating or trying to change the names of the types will cause issues. Using undefined will very likely cause a problem, too.

Additionally, performance is a serious concern. The tests will take only a few milliseconds to run, but the compilation will take quite a bit of time. Please pay attention to the time it takes to run your tests.

Good luck!
FUNDAMENTALSMATHEMATICSALGORITHMSNUMBERS
-}
{-sample test
module Kata.AdditionCommutes.Examples where

import Kata.AdditionCommutes
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "My own tests" $ do
    it "My first test" $ do
      "Write your own tests here!" `shouldNotBe` "Good luck!"
-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}

-- module Kata.AdditionCommutes
--   ( plusCommutes ) where

-- import Kata.AdditionCommutes.Definitions
--   ( Z, S
--   , Natural(..), Equal(..)
--   , (:+:))

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!-- | The natural numbers, encoded in types.
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


{-my
-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS $ reflexive n

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS n) = EqlS $ symmetric n

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS n) (EqlS m) = EqlS $ transitive n m 

-- -- | Peano definition of addition.
-- type family (:+:) (n :: *) (m :: *) :: *
-- type instance Z :+: m = m
-- type instance S n :+: m = S (n :+: m)
-- plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
-- plusCommutes NumZ NumZ = EqlZ
-- plusCommutes NumZ (NumS b) = EqlS (plusCommutes NumZ b)
-- plusCommutes (NumS a) b =  EqlS (plusCommutes a b)

cacheS :: Natural a -> Natural b -> Equal (S (a :+: b))  (a :+: S b)
cacheS NumZ b = EqlS $ reflexive b 
cacheS (NumS a) b = EqlS $ cacheS a b

plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ = EqlZ
plusCommutes NumZ (NumS b) = EqlS $ plusCommutes NumZ b
plusCommutes (NumS a) b = transitive  (EqlS $ plusCommutes a b) (cacheS b a) 
-}
-- check :: Natural a -> Natural b -> Natural (a :+: b)
-- check NumZ b = b
-- check (NumS a) b = NumS (check a b)

-- plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
-- plusCommutes a b = transitive ab ba
--   where ab = reflexive $ check a b 
--         ba = symmetric $ reflexive $ check b a
-- This is the proof that the kata requires.
-- | a + b = b + a
-- plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
-- plusCommutes NumZ m = reflexive m
-- plusCommutes (NumS n) m = EqlS $ plusCommutes n m

-- This is the proof that the kata requires.
-- | a + b = b + a
-- type family (:+:) (n :: *) (m :: *) :: *
-- type instance Z :+: m = m
-- type instance S n :+: m = S (n :+: m)
-- plusn :: Natural a -> Natural b -> Equal c c
-- plusn NumZ NumZ = EqlZ
-- plusn NumZ (NumS m) = EqlS $ plusn NumZ m
-- plusn (NumS n) (NumS m) = EqlS $ plusn n (NumS m)
-- plusn :: Natural a -> Natural b -> Natural (a :+: b)
-- plusn NumZ NumZ = NumZ
-- plusn NumZ (NumS b) = NumS $ plusn NumZ b
-- plusn (NumS a) b = NumS $ plusn a b

-- plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
-- plusCommutes a b = (reflexive $ plusn a b)
-- plusCommutes NumZ NumZ = EqlZ
-- plusCommutes NumZ (NumS m) = EqlS $ plusCommutes NumZ m
-- plusCommutes (NumS n) (NumS m) = EqlS (plusCommutes n (NumS m))
-- plusCommutes NumZ (NumS m) = EqlS $ plusCommutes NumZ m
-- plusCommutes (NumS n) NumZ = EqlS $ plusCommutes n NumZ
--plusCommutes (NumS $ NumS n) m
--plusCommutes n (NumS $ NumS m)
  --EqlS $ symmetric (plusCommutes n m)--EqlS $ EqlS $ plusCommutes n m

-- For reference, here are the definitions, if you
-- want to copy them into an IDE:
{-

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

-}

{-XxDan10xX answer
plus :: Natural a -> Natural b -> Natural (a :+: b)
plus NumZ b = b
plus (NumS a) b = NumS $ plus a b

eq :: Natural a -> Natural b -> Equal a b
eq NumZ NumZ = EqlZ
eq (NumS a) (NumS b) = EqlS $ eq a b

-- Well... I spent many hours trying to prove this and came up with this retarded solution.
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes a b = eq (plus a b) (plus b a)
-}

{-aditya7iyengar, Voile, DIDIx13, rodolfo.sures answer
-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS eq) = EqlS (symmetric eq)

-- | if a = b and b = c, then a = c.
(<=>) :: Equal a b -> Equal b c -> Equal a c
EqlZ <=> EqlZ = EqlZ
(EqlS ab) <=> (EqlS bc) = EqlS (ab <=> bc)

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ = EqlZ
plusCommutes NumZ (NumS b) = EqlS (plusCommutes NumZ b)
plusCommutes (NumS a) NumZ = EqlS (plusCommutes a NumZ)
plusCommutes sa@(NumS a) sb@(NumS b) = EqlS ((plusCommutes a sb) <=> (EqlS (plusCommutes b a)) <=> (plusCommutes sa b))
-}