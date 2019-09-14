{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

import GHC.TypeLits

minfree :: [Nat] -> Nat
minfree = head

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (judge vs) us

judge :: Eq a => [a] -> a -> Bool
judge (x:xs) it = x /= it && judge xs it--if x==it then False else judge xs it
judge _ _ = True
-- judge (x:xs) it = if x==it then False else judge xs it
-- judge _ _ = True

--用逐个判断，遇到false就为false，否则为true
