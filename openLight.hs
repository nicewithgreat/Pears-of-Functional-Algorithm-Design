{-
有n盏灯，编号为1~n。第1个人把所有灯打开，
第2个人按下所有编号为2的倍数开关（这些灯将被关掉），
第3个人按下所有编号为3的倍数的开关，以此类推。一共有k个人，
问最后有哪些灯开着？输入n和k，输出开着的灯的编号。k<=n<=100

样例输入    7  3
样例输出    1  5   6   7
-}

module Main (main) where


main :: IO ()
main = do
    s <- getLine
    let (n:k:_) = words s
    putStrLn $ result (openlight (read n) (read k))

result :: [Int] -> String
result (a:ax) = if null ax then show a else show a ++ " " ++ result ax

openlight :: Int -> Int -> [Int]
openlight n 0 = []
openlight n k = 
    let lightON     = openlight n (k-1) --已开
        lightOFF    = [x | x <- [1..n] , not $ elem x lightON] --尚未开
        on  = [x | x <- lightOFF ,  x `mod` k == 0 ]    --打开
        off = [x | x <- lightON ,   x `mod` k /= 0 ]    --关闭
    in  [x | x <- [1..n] , elem x (on ++ off)]  --排序