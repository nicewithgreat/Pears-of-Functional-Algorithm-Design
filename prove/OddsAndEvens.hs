{-
Looking for a challenge? The kata A+B=B+A? Prove it! is a great next step to this kata.
What's this kata about?
In this kata, you will prove, via types multiple facts about even and odd numbers. These facts include:

If n is even, then n+1 is odd.
If n is odd, then n+1 is even.
If n is even and m is odd, then n+m is odd.
...and so on.

This will require knowledge of type families, datakinds, and GADTs.

How do we prove something with types?
Functions can be read as implications. Think of the type A -> B as reading 'if we have an A, then we can get a B'. For this reason, if we think of types as propositions rather than data, then we can build proofs using functions between them, where A -> B is read as 'A implies B'.

In this kata, three datatypes are defined:

-- | The natural numbers.
data Nat = Z | S Nat

-- | The axioms of the even numbers.
data Even (n :: Nat) where
  -- | Axiom: zero is even.
  ZeroEven :: Even Z
  -- | Axiom: if n is even, then n+2 is even.
  NextEven :: Even n -> Even (S (S n))

-- | The axioms of the odd numbers.
data Odd (n :: Nat) where
  -- | Axiom: one is odd.
  OneOdd :: Odd (S Z)
  -- | Axiom: if n is odd, then n+2 is odd.
  NextOdd :: Odd n -> Odd (S (S n))
Now we have the axioms built. Here they are represented as data constructors, but in the corresponding proof we can imagine them as the base assumptions from which we can build our proofs.

Once we have type families for certain operations built, we can build proofs like so:

evenPlusOdd :: Even n -> Odd m -> Odd (Add m n)
evenPlusOdd = -- (proof here)
The initial solution will provide type signatures for all the proofs you will need to complete. Good luck!

(Remember: the principle challenge in this kata is getting it to typecheck. The rest is easy, as long as you don't use undefined.)

FUNDAMENTALSARITHMETICMATHEMATICSALGORITHMSNUMBERS
-}
{-simple test-}
-- {-# LANGUAGE GADTs #-}
-- module Tests where

-- import OddsAndEvens as S
-- import Test.Hspec

-- main = hspec $ do
--   describe "(*)PlusOne" $ do
--     it "evenPlusOne works" $ do
--       fromOdd (S.evenPlusOne two) `shouldBe` 3
--       fromOdd (S.evenPlusOne six) `shouldBe` 7
--       fromOdd (S.evenPlusOne eight) `shouldBe` 9
--     it "oddPlusOne works" $ do
--       fromEven (S.oddPlusOne one) `shouldBe` 2
--       fromEven (S.oddPlusOne five) `shouldBe` 6
--       fromEven (S.oddPlusOne nine) `shouldBe` 10
--   describe "(*)Plus(*)" $ do
--     it "evenPlusEven works" $ do
--       fromEven (S.evenPlusEven zero four) `shouldBe` 4
--     it "evenPlusOdd works" $ do
--       fromOdd (S.evenPlusOdd four three) `shouldBe` 7
--     it "oddPlusOdd works" $ do
--       fromEven (S.oddPlusOdd nine one) `shouldBe` 10
--     it "oddPlusEven works" $ do
--       fromOdd (S.oddPlusEven one two) `shouldBe` 3
--   describe "(*)Times(*)" $ do
--     it "evenTimesEven works" $ do
--       fromEven (S.evenTimesEven four six) `shouldBe` 24
--     it "evenTimesOdd works" $ do
--       fromEven (S.evenTimesOdd zero nine) `shouldBe` 0
--     it "oddTimesOdd works" $ do
--       fromOdd (S.oddTimesOdd nine one) `shouldBe` 9
--     it "oddTimesEven works" $ do
--       fromEven (S.oddTimesEven three four) `shouldBe` 12

-- -- Representations to Integers
-- fromEven :: Even n -> Int
-- fromEven ZeroEven = 0
-- fromEven (NextEven n) = 2 + fromEven n
-- fromOdd :: Odd n -> Int
-- fromOdd OneOdd = 1
-- fromOdd (NextOdd n) = 2 + fromOdd n

-- -- Numbers for help during tests
-- zero = ZeroEven
-- one = OneOdd
-- two = NextEven zero
-- three = NextOdd one
-- four = NextEven two
-- five = NextOdd three
-- six = NextEven four
-- seven = NextOdd five
-- eight = NextEven six
-- nine = NextOdd seven
-- ten = NextEven eight

{-# LANGUAGE GADTs, DataKinds,
             TypeFamilies, UndecidableInstances #-}

module OddsAndEvens where

-- | The natural numbers.
data Nat = Z | S Nat

-- | The axioms of even numbers.
data Even (a :: Nat) :: * where
  -- | Zero is even.
  ZeroEven :: Even Z
  -- | If n is even, then n+2 is even.
  NextEven :: Even n -> Even (S (S n))

-- | The axioms of odd numbers.
data Odd (a :: Nat) :: * where
  -- | One is odd.
  OneOdd :: Odd (S Z)
  -- | If n is odd, then n+2 is odd.
  NextOdd :: Odd n -> Odd (S (S n))

-- | Proves that if n is even, n+1 is odd.
-- Notice how I use the axioms here.
evenPlusOne :: Even n -> Odd (S n)
evenPlusOne ZeroEven = OneOdd
evenPlusOne (NextEven n) = NextOdd (evenPlusOne n)

-- | Proves that if n is odd, n+1 is even.
oddPlusOne :: Odd n -> Even (S n)
oddPlusOne OneOdd = NextEven ZeroEven
oddPlusOne (NextOdd n) = NextEven  (oddPlusOne n)

-- | Adds two natural numbers together.
-- Notice how the definition pattern matches.
type family   Add (n :: Nat) (m :: Nat) :: Nat
type instance Add Z m = m
type instance Add (S n) m = S (Add n m)

-- | Proves even + even = even
-- Notice how the pattern matching mirrors `Add`s definition.
evenPlusEven :: Even n -> Even m -> Even (Add n m)
evenPlusEven ZeroEven m = m
evenPlusEven (NextEven n) m = NextEven (evenPlusEven n m)

-- | Proves odd + odd = even
oddPlusOdd :: Odd n -> Odd m -> Even (Add n m)
oddPlusOdd OneOdd m = oddPlusOne m
oddPlusOdd (NextOdd n) m = NextEven (oddPlusOdd n m) 

-- | Proves even + odd = odd
evenPlusOdd :: Even n -> Odd m -> Odd (Add n m)
evenPlusOdd ZeroEven m = m
evenPlusOdd (NextEven n) m = NextOdd (evenPlusOdd n m) 

-- | Proves odd + even = odd
oddPlusEven :: Odd n -> Even m -> Odd (Add n m)
--oddPlusEven n m = evenPlusOdd m n 
oddPlusEven OneOdd n = evenPlusOne n
oddPlusEven (NextOdd m) n = NextOdd (oddPlusEven m n) 


-- | Multiplies two natural numbers.
type family   Mult (n :: Nat) (m :: Nat) :: Nat
type instance Mult Z m = Z
type instance Mult (S n) m = Add (Mult n m) m-- TODO: Mult n m

-- | Proves even * even = even
evenTimesEven :: Even n -> Even m -> Even (Mult n m)
evenTimesEven ZeroEven m = ZeroEven
evenTimesEven (NextEven n) m = evenPlusEven (evenPlusEven (evenTimesEven n m) m) m

-- | Proves odd * odd = odd
oddTimesOdd :: Odd n -> Odd m -> Odd (Mult n m)
oddTimesOdd OneOdd m = m
oddTimesOdd (NextOdd n) m = evenPlusOdd (oddPlusOdd (oddTimesOdd n m) m) m

-- | Proves even * odd = even
evenTimesOdd :: Even n -> Odd m -> Even (Mult n m)
evenTimesOdd ZeroEven m = ZeroEven
evenTimesOdd (NextEven n) m = oddPlusOdd (evenPlusOdd (evenTimesOdd n m) m) m

-- | Proves odd * even = even
oddTimesEven :: Odd n -> Even m -> Even (Mult n m)
oddTimesEven OneOdd m = m
oddTimesEven (NextOdd n) m = evenPlusEven (evenPlusEven (oddTimesEven n m) m) m