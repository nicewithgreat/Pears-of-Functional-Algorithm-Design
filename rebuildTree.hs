module Main (main) where

-- 重建二叉树
main :: IO ()
main = do
    dlr <- getLine
    ldr <- getLine
    putStrLn $ lrd $ buildtree dlr ldr
    

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving(Show,Read)
{-
    dlr 先序 中左右
    ldr 中序 左中右
    lrd 后序 左右中
-}
lrd :: Tree a -> [a]
lrd (Node d l r) = lrd l ++ lrd r ++ [d] 
lrd EmptyTree = []

buildtree :: Ord a => [a] -> [a] -> Tree a
buildtree (d:dlr) ldr = Node d ltree rtree
    where   ltree = buildtree l_dlr l_ldr
            rtree = buildtree r_dlr r_ldr
            (l_ldr,r_ldr) = split d ldr
            l_dlr = [x | x <- dlr , elem x l_ldr]
            r_dlr = [x | x <- dlr , elem x r_ldr]
buildtree _ _ = EmptyTree

split :: Ord a => a -> [a] -> ([a],[a])
split j (a:ax)  | j == a = ([],ax)
                | otherwise = (a:l,r)
                where (l,r) = split j ax