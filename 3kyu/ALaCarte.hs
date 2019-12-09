{-
In the synonymous functional pearl, Wouter Swierstra demonstrates one way to solve the expression problem. This kata will closely follow his exposition.

The expression problem as first stated by Wadler is as follows:

The goal is to define a data type by cases, where one can add new cases to the data type and new functions over the data type, without recompiling existing code, and while retaining static type safety.

Now by example, consider a very simple ADT.

data Expr = Lit Int | Add Expr Expr
It is very easy to define different interpretations of this datatype, here are just two examples.

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

pretty :: Expr -> String
pretty (Lit n) = show n
pretty (Add e1 e2) = pretty e1 ++ "+" ++ pretty e2
On the other hand, if we wanted to extend Expr to include multiplication.

data Expr = Lit Int | Add Expr Expr | Mult Expr Expr
Then we have to go back add a clause to where the interpreter was originally defined to add a new clause to recognise Mult. This could be problematic if eval is defined inside a library to which you don't have access.

Simply, Swierstra's solution is instead of defining an evaluation function, we define an evaluation type class. For each constructor which admits a definition for that operator, we define an instance of the type class.

In this kata we will walk through the first section of Data Type à la Carte.

This kata was broken, but now it isn't!

FUNDAMENTALSFUNCTIONAL PROGRAMMINGDECLARATIVE PROGRAMMING

-}
{--simple test
-- {-# OPTIONS_GHC -fno-warn-deprecated-flags #-}
-- {-# OPTIONS_GHC -fno-warn-tabs #-}
-- {-# LANGUAGE TypeOperators, DeriveFunctor, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-- module ALaCarte.ExampleTests where

-- import Test.Hspec
-- import ALaCarte hiding (expr1, expr2)

-- expr1 :: Expr (Add :+: Lit)
-- expr1 = add (lit 5) (lit 6)

-- expr2 :: Expr (Add :+: Lit :+: Mult)
-- expr2 = mult (add (lit 5) (lit 6)) (lit 2)

-- main = hspec $ do
--   describe "Examples" $ do
--     it "eval expr1 == 11" $ do
--       eval expr1 `shouldBe` 11
--     it "eval expr2 == 22" $ do
--       eval expr2 `shouldBe` 22
--     it "pretty expr1 == (5+6)" $ do
--       pretty expr1 `shouldBe` "(5+6)"
--     it "pretty expr2 == ((5+6)*2)" $ do
--       pretty expr2 `shouldBe` "((5+6)*2)"
--}

{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TypeOperators, DeriveFunctor, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, OverlappingInstances #-}
module ALaCarte where

-- Definitions
data Expr f = In (f (Expr f))

-- We define a separate data type for each constructor we want to use
-- then we can combine them together using the (:+:) operator to make
-- our data types à la carte.

data Lit a = Lit Int
data Add a = Add a a

-- Coproduct
data (f :+: g) e = Inl (f e) | Inr (g e)
infixr 1 :+:

-- By defining functor instances we can write a generic fold operator
-- which will be useful to evaluate our expressions.

instance Functor Lit where
  fmap f (Lit a) = Lit a

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2) 

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl a) = Inl (fmap f a)
  fmap f (Inr b) = Inr (fmap f b)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In e) = f (fmap (foldExpr f) e)

-- Now we can write a simple interpreter. Your definitions should correspond
-- closely with the definition for the old interpreter given in the description.

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Lit where
  evalAlgebra (Lit x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl l) = evalAlgebra l
  evalAlgebra (Inr r) = evalAlgebra r
  
eval :: Eval f => Expr f -> Int
eval = foldExpr evalAlgebra

-- HINT: Use foldExpr

-- The problem is that it is painful to write expressions. 
-- This is how you would write 5+6

pain :: Expr (Lit :+: Add)
pain = In (Inr (Add (In (Inl (Lit 5))) (In (Inl (Lit 6)))))

-- Injection
-- To ease writing expressions, we will now define a type class
-- which will choose the right constructors for us. Think of the sub :<: sup to say that
-- sub is a subtype of sup. 

-- It might also help to think of :+: as the cons operator for a type level list.
-- Then the type class can be viewed as searching for the correct injection by
-- searching through the list for the correct type.

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a


-- Reflexivity
instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) =>  f :<: (f :+: g) where
  inj = Inl

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  
-- Note: This part requires overlapping instances, this is safe as long as :+: associates to the right.
-- A modern implementation would use type families. 

-- Then we can use this type class to write smart constructors.

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

lit :: (Lit :<: f) => Int -> Expr f
lit n = inject $ Lit n

add :: (Add :<: f) => Expr f -> Expr f -> Expr f
add e1 e2 = inject $ Add e1 e2

-- Then as long as we specify the type, writing expressions is easy.

expr :: Expr (Add :+: Lit)
expr = add (lit 5) (lit 6)

-- > eval expr
-- 11

-- We can add multiplication very easily.

data Mult a = Mult a a deriving Functor

instance Eval Mult where
  evalAlgebra (Mult x y) = x * y

mult :: (Mult :<: f) => Expr f -> Expr f -> Expr f
mult e1 e2 = inject $ Mult e1 e2

-- We must specify the type of expressions
expr2 :: Expr (Mult :+: Add :+: Lit)
expr2 = mult (add (lit 5) (lit 6)) (lit 2)

-- > eval expr
-- 22

-- Adding a new interpreter
-- To add a pretty printer, we define a new type class in much the
-- same way as for the first interpreter. 

class Functor f => Pretty f where
  render :: Pretty g => f (Expr g) -> String

pretty :: Pretty f => Expr f -> String
pretty (In t) = render t

instance Pretty Lit where
  render (Lit i) = show i

instance Pretty Add where
  render (Add x y) = "(" ++ pretty x ++ "+" ++ pretty y ++ ")"

instance Pretty Mult where
  render (Mult x y) = "(" ++ pretty x ++ "*" ++ pretty y ++ ")"

instance (Pretty f, Pretty g) => Pretty (f :+: g) where
  render (Inl x) = render x
  render (Inr y) = render y
 
-- > pretty expr1
-- "(5+6)"
-- > pretty expr2
-- "((5+6)*2)"