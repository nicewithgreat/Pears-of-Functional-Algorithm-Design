{-
Can you make algebraic data types even more functional? Of course!

Scott encoding represents data as funtion that apply their argument (also a function) to their value. This approach is similar to using pattern matching on regular ADTs to extract and use their content.

You are given types representing the Scott encoding of four common Haskell types: Maybe, lists, Either and pairs (2-tuples).

Your task is to implement conversion functions between these regular types and their Scott encoding.

In addition, you will have to implement the following common operations using the provided Scott-encoded data types:

fst and snd, functions to extract the content of a pair
swap, a function that exchanges the content of a pair
curry, a function to turn functions of pairs into functions of two arguments
uncurry, a function to turn functions of two arguments into functions of pairs
isJust and isNothing, predicates testing wether a Maybe contains data
isLeft and isRight, predicates testing which side is contained in an Either
cons, a function to prepend an element to a list
concat, a function to contanetate two lists
catMaybes, a function to flatten a list of Maybes by removing Nothings
null, a predicate testing wether a list is empty
length, a function returning the number of elements in a list
map, a function to transform the contents of a list according to a given function
zip, a funtion to merge two lists into a list of pairs
partition, a function that splits a list of Eithers in a pair of Lefts and Rights
foldl and foldr, functions to reduce a list to a single value by successive application of a given function
take, a function to limit a list to a number of initial elements
FUNDAMENTALSFUNCTIONAL PROGRAMMINGDECLARATIVE PROGRAMMING
-}
{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair sp = runPair sp (,)
fromPair :: (a,b) -> SPair a b
fromPair (a,b) = SPair {runPair = \f->f a b} 
fst :: SPair a b -> a
fst ab = a
    where (a,_) = toPair ab
snd :: SPair a b -> b
snd ab = b
    where (_,b) = toPair ab
swap :: SPair a b -> SPair b a
swap ab = fromPair (b,a)
    where (a,b) = toPair ab
curry :: (SPair a b -> c) -> (a -> b -> c)
curry f = \a b -> f $ fromPair (a,b)
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = \ab -> f (fst ab) (snd ab)

--newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
toMaybe :: SMaybe a -> Maybe a
toMaybe a = runMaybe a Nothing Just
fromMaybe :: Maybe a -> SMaybe a
fromMaybe (Just a) = SMaybe {runMaybe = \b f->f a}
fromMaybe Nothing = SMaybe {runMaybe = \b f->b}
isJust :: SMaybe a -> Bool
isJust a = case toMaybe a of
                Nothing -> False
                _ -> True
isNothing :: SMaybe a -> Bool
isNothing = not . isJust
catMaybes :: SList (SMaybe a) -> SList a
catMaybes lma | null lma =  fromList []
              | otherwise = if isJust x 
                            then let (Just a) = toMaybe x in cons a (catMaybes $ fromList xs)
                            else catMaybes $ fromList xs
    where (x:xs) = toList lma

--newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
toEither :: SEither a b -> Either a b
toEither ab = runEither ab Left Right
fromEither :: Either a b -> SEither a b
fromEither (Left a) = SEither {runEither = \l r -> l a}
fromEither (Right b) = SEither {runEither = \l r -> r b }
isLeft :: SEither a b -> Bool
isLeft ab = judge $ toEither ab 
judge :: Either a b -> Bool
judge (Left a) = True
judge _ = False
isRight :: SEither a b -> Bool
isRight = not . isLeft
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition leab  | null leab =  fromPair (fromList [],fromList [])
                | otherwise =   if isLeft x 
                                then let (Left a)=toEither x in fromPair ((cons a ax),bx)
                                else let (Right b)=toEither x in fromPair (ax,(cons b bx))
    where   (x:xs) = toList leab
            (ax,bx) = toPair $ partition $ fromList xs


--newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
toList :: SList a -> [a]
toList sa = runList sa [] (\a la -> (a : toList la))
fromList :: [a] -> SList a
fromList [] = SList {runList = \b f -> b}
fromList (a:ax) = SList {runList = \b f -> f a (fromList ax)}
cons :: a -> SList a -> SList a
cons a la = fromList (a : toList la)
concat :: SList a -> SList a -> SList a
concat la lb = foldr cons lb la

null ::  SList a -> Bool
null a = case toList a of
            [] -> True
            _ -> False
length ::  SList a -> Int
length la = sum . toList $ map (const 1) la
map :: (a -> b) -> SList a -> SList b
map f la    | null la = fromList []
            | otherwise = cons (f a) (map f (fromList ax)) 
                where (a:ax) = toList la 
zip :: SList a -> SList b -> SList (SPair a b)
zip la lb   | null la = fromList []
            | null lb = fromList []
            | otherwise = cons (fromPair (a,b)) (zip (fromList ax) (fromList bx))
    where   (a:ax) = toList la
            (b:bx) = toList lb
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f b la    | null la = b
                | otherwise = foldl f (f b a) (fromList ax)
    where (a:ax) = toList la
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f b la    | null la = b
                | otherwise = f a (foldr f b (fromList ax)) 
    where (a:ax) = toList la 
take :: Int -> SList a -> SList a
take 0 la = fromList []
take n la   | null la = fromList []
            | otherwise = cons a (take (n-1) (fromList ax))
    where (a:ax) = toList la 

{--- Josef-Vonasek, MCLeon, DeDow, User221074, rodo.sures
-- {-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
-- module ScottEncoding where

-- import Prelude hiding (null, length, map, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

-- newtype SMaybe a         = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
-- newtype SList a          = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
-- newtype SEither a b      = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
-- newtype SPair a b       = SPair { runPair :: forall c. (a -> b -> c) -> c }

-- toPair                  :: SPair a b -> (a,b)
-- fromPair                :: (a,b) -> SPair a b
-- fst                     :: SPair a b -> a
-- snd                     :: SPair a b -> b
-- swap                    :: SPair a b -> SPair b a
-- curry                   :: (SPair a b -> c) -> (a -> b -> c)
-- uncurry                 :: (a -> b -> c) -> (SPair a b -> c)

-- mkPair a b              = SPair $ \fun -> fun a b
-- toPair pair             = runPair pair (,)
-- fromPair (a,b)          = mkPair a b
-- fst pair                = runPair pair $ \a b -> a
-- snd pair                = runPair pair $ \a b -> b
-- swap pair               = runPair pair $ \a b -> mkPair b a
-- curry fun a b           = fun $ mkPair a b
-- uncurry fun p           = runPair p fun

-- toMaybe                 :: SMaybe a -> Maybe a
-- fromMaybe               :: Maybe a -> SMaybe a
-- isNothing               :: SMaybe a -> Bool
-- isJust                  :: SMaybe a -> Bool
-- catMaybes               :: SList (SMaybe a) -> SList a

-- some a                  = SMaybe $ \b fun -> fun a
-- none                    = SMaybe $ \b fun -> b
-- fromMaybe Nothing       = none
-- fromMaybe (Just a)      = some a
-- toMaybe maybe           = runMaybe maybe Nothing Just
-- isJust maybe            = runMaybe maybe False (const True)
-- isNothing maybe         = runMaybe maybe True (const False)
-- catMaybes               = foldr (\mb as -> runMaybe mb as (\a -> cons a as)) empty

-- toEither                :: SEither a b -> Either a b
-- fromEither              :: Either a b -> SEither a b
-- isLeft                  :: SEither a b -> Bool
-- isRight                 :: SEither a b -> Bool
-- partition               :: SList (SEither a b) -> SPair (SList a) (SList b)

-- toEither either         = runEither either Left Right
-- fromEither (Left a)     = SEither $ \l r -> l a
-- fromEither (Right b)    = SEither $ \l r -> r b
-- isLeft either           = runEither either (const True) (const False)
-- isRight either          = runEither either (const False) (const True)
-- partition list          = mkPair lpart rpart
--     where
--     lpart = catMaybes . map (\e -> runEither e some (const none)) $ list
--     rpart = catMaybes . map (\e -> runEither e (const none) some) $ list

-- toList                  :: SList a -> [a]
-- fromList                :: [a] -> SList a
-- cons                    :: a -> SList a -> SList a
-- concat                  :: SList a -> SList a -> SList a
-- null                    :: SList a -> Bool
-- length                  :: SList a -> Int
-- map                     :: (a -> b) -> SList a -> SList b
-- zip                     :: SList a -> SList b -> SList (SPair a b)
-- foldl                   :: (b -> a -> b) -> b -> SList a -> b
-- foldr                   :: (a -> b -> b) -> b -> SList a -> b
-- take                    :: Int -> SList a -> SList a

-- cons a as               = SList $ \b f -> f a as
-- empty                   = SList $ \b f -> b
-- toList                  = foldr (:) []
-- fromList []             = empty
-- fromList (a:as)         = cons a (fromList as)
-- concat as bs            = foldr cons bs as
-- null list               = runList list True $ \_ _ -> False
-- length                  = foldl (\b _ -> b+1) 0
-- map fun                 = foldr (cons . fun) empty
-- foldl fun b list        = runList list b $ \a as -> foldl fun (fun b a) as
-- foldr fun b list        = runList list b $ \a as -> fun a (foldr fun b as)
-- take i list             = foldr foo (\i -> empty) list i
--     where 
--     foo _ _ 0 = empty
--     foo a f i = cons a (f (i-1))
-- zip as bs               = foldr foo (\bs -> empty) as bs
--     where
--     foo a f bs = runList bs empty $ \b bs -> cons (mkPair a b) (f bs)



--}