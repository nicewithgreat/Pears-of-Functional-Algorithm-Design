import Data.List
--最少硬币问题
question = ([(5,3),(4,5),(2,3),(1,3)],13)
answer = if as == 100000
            then -1
            else as
        where as = deal question

deal (xs,m) = findl xs' m
    where xs' = qSort xs

qSort :: [(Int,Int)] -> [(Int,Int)]
qSort []=[]
qSort (x:xs)=qSort l ++ [x] ++qSort r
    where (l,r)=partition (\y -> (fst x) < (fst y)) xs


findl :: [(Int,Int)] -> Int -> Int
findl _ 0 = 0
findl xs l = findmin (map (count l $) (diving xs l))

findmin :: [Int] -> Int
findmin [] = 100000
findmin (x:xs)  | x < x' = x
                | otherwise = x' 
        where x' = findmin xs

count l xs = 
    if j 
        then 1 + findl xs' (l-l')
        else -1
    where (j,l',xs') = fix xs l

diving :: [(Int,Int)] -> Int -> [[(Int,Int)]]
diving [] _ = []
diving vn@((a,b):xs) n  | b>0 && a<=n = vn : diving xs n
                        | otherwise = diving xs n

fix :: [(Int,Int)] -> Int -> (Bool,Int,[(Int,Int)])
fix ((a,b):xs) n    | b>0 && a<=n = (True,a,(a,b-1):xs)
                    | otherwise = (j,a',(a,b):xs')
                    where (j,a',xs') = fix xs n
fix _ _ = (False,0,[])
