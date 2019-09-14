{-
n(n<20)个人站成一圈，逆时针编号为1~n。
有两个官员，A从1开始逆时针数，B从n开始顺时针数。
在每一轮中，官员A数k个就停下来，官员B数m个就停下来
（注意有可能两个官员停在同一个人上）。
接下来被官员选中的人（1个或者2个）离开队伍。
输入 n，k，m 输出每轮里被选中的人的编号
样例输入   
n=10 k=4 m=3 
样例输出   
4 8 9 5 3 1 2 6 10 7
-}

module Main (main) where


main :: IO ()
main = do
    s <- getLine
    let (n,k,m) = read3 s
    putStrLn $ result (abloop [1..n] k m n (True,0,0))

result :: [Int] -> String
result (a:ax) = if null ax then show a else show a ++ " " ++ result ax

--读 n k m
read3 :: String -> (Int , Int , Int)
read3 s = (readint n , readint k , readint m)
    where (n:k:m:_) = words s

--读整型
readint :: String -> Int
readint (s:xs) = if null xs then readchar s else (readchar s) * 10 + readint xs
    where readchar  s
                    | s == '1' = 1
                    | s == '2' = 2
                    | s == '3' = 3
                    | s == '4' = 4
                    | s == '5' = 5
                    | s == '6' = 6
                    | s == '7' = 7
                    | s == '8' = 8
                    | s == '9' = 9
                    | otherwise = 0
 
{-测试用例
*Main> abloop [1..10] 4 3 10 (True,0,0)
[4,8,9,5,3,1,2,6,10,7]
-}
--转圈圈
abloop :: [Int] -> Int -> Int -> Int -> (Bool , Int , Int) -> [Int]
abloop [] _ _ _ _ = []
abloop [n] _ _ _ _ = [n]
abloop ns k m long judge =  if a==b 
                            then a : (abloop ns' k m (long-1) judge')
                            else a : b : (abloop ns' k m (long-2) judge')
    where   (ap,bp) = position judge long
            judge' = step (k,m) (ap,bp) long
            (a,b,ns') = onlyloop ns judge'


--数组 -> step处理后生成的 -> (a,b,处理后数组,起点到最左边坐标的长度)
onlyloop :: [Int] -> (Bool , Int , Int) -> (Int , Int , [Int])
onlyloop (n:ns) (_,0,0) = (n,n,ns)
onlyloop (n:ns) (_,-1,0) = (n,n,ns)
onlyloop (n:ns) (j,0,b) = if j then (n,b',ns') else (a',n,ns')
    where (a',b',ns') = onlyloop ns (j,-1,b-1)
onlyloop (n:ns) (j,-1,b) = (a',b',n:ns')
    where (a',b',ns') = onlyloop ns (j,-1,b-1)
onlyloop (n:ns) (j,a,b) = (a',b',n:ns')
    where (a',b',ns') = onlyloop ns (j,a-1,b)


-- ab步长 -> ab位置 -> s长度 -> (a是否为左边,最左边坐标，距离第一个的坐标的长度)
step :: (Int , Int) -> (Int , Int) -> Int -> (Bool , Int , Int)
step (a,b) (ap,bp) s =  if judge
                        then (judge , ap' , bp'-ap')
                        else (judge , bp' , ap'-bp')
        where   judge = ap' < bp'
                ap' = (ap + a + s) `mod` s
                bp' = (bp - b + s) `mod` s

--(a是否为左边,最左边坐标，距离第一个的坐标的长度) -> 现数组长度 -> (现ab的位置)
position :: (Bool , Int , Int) -> Int -> (Int , Int)
position (aNotl,left,l) long =  if l==0 
                                then abwhere 1
                                else abwhere l
                                where abwhere l' =  if aNotl
                                                    then ((left-1 + long) `mod` long    , (left-1+l' + long) `mod` long)
                                                    else ((left-2+l' + long) `mod` long , left)
                                              