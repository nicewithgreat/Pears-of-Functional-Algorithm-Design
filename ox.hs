{-
给出一个由O和X组成的串，统计得分。每个O的得分为目前连续出现O的个数，X的得分为0。
样例输入
    OOXXOXXOOO
样例输出
     10=（1+2+0+0+1+0+0+1+2+3
-}

module Main (main) where


main :: IO ()
main = do
    input <- getLine
    print $ (qsum input 0)


qsum :: String -> Int -> Int
qsum (x:xs) pt = if x == 'O' 
    then next + qsum xs next 
    else qsum xs 0
    where next = pt + 1
qsum _ _ = 0
