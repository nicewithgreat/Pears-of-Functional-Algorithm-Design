{-
Inspired from the a+b=b+a kata, and this one is much simpler.

The definition of natural numbers are all the same, the only difference is the theorem to be proved. This time we're gonna prove the plus associative law.

FUNDAMENTALSNUMBERSMATHEMATICSALGORITHMS
-}
{--sample test
-- {-# LANGUAGE GADTs           #-}
-- {-# LANGUAGE TypeFamilies    #-}
-- {-# LANGUAGE TypeOperators   #-}

-- module Kata.AdditionAssoc.TestSpec (main) where

-- import Kata.AdditionAssoc
-- import Kata.AdditionAssoc.Definitions

-- import Test.Hspec

-- -- | Verify that the functions' signature is correct:
-- solution :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
-- solution = plusAssoc

-- main :: IO ()
-- main = hspec $ do
--   describe "Proof checking" $ do
--     it "Doesn't use any unsafe modules" $
--       hidden [Module "Unsafe.Coerce"]
--}

{- Preloaded code, might be helpful for
   doing this Kata locally
-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
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



-- Helpers

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS $ reflexive n

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS n) = EqlS $ symmetric n


-- | if a = b and b = c, then a = c.
(<=>) :: Equal a b -> Equal b c -> Equal a c
EqlZ <=> EqlZ = EqlZ
(EqlS ab) <=> (EqlS bc) = EqlS (ab <=> bc)

plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ NumZ = EqlZ
plusCommutes NumZ (NumS b) = EqlS (plusCommutes NumZ b)
plusCommutes (NumS a) NumZ = EqlS (plusCommutes a NumZ)
plusCommutes sa@(NumS a) sb@(NumS b) = EqlS ((plusCommutes a sb) <=> (EqlS (plusCommutes b a)) <=> (plusCommutes sa b))


-- This is the proof that the kata requires.
-- | a + (b + c) = (a + b) + c
{-
a + (b + S c) = 
a + S (b + c) =
S(a + (b + c))

(a + b) + S c =
S((a + b) + c)
-}
-- plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
-- plusAssoc a NumZ NumZ = EqlZ
-- plusAssoc a (NumS b) NumZ = EqlS $ plusAssoc a b NumZ
-- plusAssoc a b (NumS c) = (fstL a b c) <=> (scdL a b c) <=> (EqlS $ plusAssoc a b c)

-- plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
-- plusAssoc a b (NumS c) = undefind

{-
1- (A+(B+S C)) = (A+S (B+C))
2- (A+S (B+C)) = S (A+(B+C))
3- S (A+(B+C)) = S ((A+B)+C)
4- S ((A+B)+C) = ((A+B)+S C)
-}
{--
--1- (A+(B+S C)) = (A+S (B+C))
--1- step1 a b c

--2- (A+S (B+C)) = S (A+(B+C))
--2- step2 a b c

--3- S (A+(B+C)) = S ((A+B)+C) 
--3- EqlS $ plusAssoc a b c

--4- S ((A+B)+C) = ((A+B)+S C)
--4- step4 a b c
--}
add :: Natural a -> Natural b -> Natural (a :+: b)
add NumZ b = b
add (NumS a) b = NumS $ add a b

cacheS :: Natural a -> Natural b -> Equal (S (a :+: b))  (a :+: S b)
cacheS NumZ b = EqlS $ reflexive b 
cacheS (NumS a) b = EqlS $ cacheS a b

adda :: Natural a -> Equal b c -> Equal (a:+:b) (a:+:c)
adda NumZ b = b
adda (NumS a) b = EqlS $ adda a b

step1 :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: S c))  (a :+: S (b :+: c))
step1 a b c = adda a $ symmetric $ cacheS b c

step2 :: Natural a -> Natural b -> Natural c -> Equal (a :+: S (b :+: c)) (S(a :+: (b :+: c)))
step2 a b c = symmetric $ cacheS a bc
  where bc = add b c

step4 :: Natural a -> Natural b -> Natural c -> Equal (S((a :+: b) :+: c)) ((a :+: b) :+: S c)
step4 a b c = cacheS ab c
  where ab = add a b

--step3
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
-- plusAssoc NumZ NumZ NumZ = EqlZ
-- plusAssoc (NumS a) NumZ NumZ = EqlS $ plusAssoc a NumZ NumZ
-- plusAssoc a (NumS b) NumZ = EqlS $ plusAssoc a b NumZ
plusAssoc a NumZ NumZ = reflexive a
plusAssoc a (NumS b) NumZ = EqlS $ symmetric $ plusAssoc a b NumZ
plusAssoc a b (NumS c) = (step1 a b c ) <=> (step2 a b c) <=> (EqlS $ plusAssoc a b c) <=> (step4 a b c)

-- final ::  Natural a -> Natural b -> Equal (a :+: (b :+: Z)) ((a :+: b) :+: Z)
-- final a b = (reflexive (add a (add b NumZ))) <=> (reflexive $ add a b) <=> (reflexive (add (add a b) NumZ))
-- fstL :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: S c)) (a :+: S (b :+: c))
-- fstL a NumZ NumZ = reflexive a
-- fstL a (NumS b) NumZ = EqlS $ fstL a b NumZ
-- fstL a b (NumS c) = fstL a (NumS b) c

-- scdL :: Natural a -> Natural b -> Natural c -> Equal (a :+: S (b :+: c)) (S((a :+: b) :+: c))


-- cacheL :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) (a :+: (b :+: c))
-- cacheL NumZ NumZ c = reflexive c
-- cacheL (NumS a) NumZ c = EqlS $ cacheL a NumZ c
-- cacheL a (NumS b) c = EqlS $ cacheL a b c


--cacheR :: Natural a -> Natural b -> Natural c -> Equal ((a :+: b) :+: c) ((a :+: b) :+: c)