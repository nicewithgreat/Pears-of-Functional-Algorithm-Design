--剪绳子，求相乘最大
main = do
    n <- getLine
    return $ cut $ read n

cut :: Int -> Int
cut 0 = 1
cut l = max l $ maxx [cut x * cut (l-x) | x <- [1..(div l 2)]] 0

maxx :: [Int] -> Int -> Int
maxx [] m = m
maxx (a:ax) m   | a > m = maxx ax a
                | otherwise = maxx ax m
                

