{-
计算机想一个单词让你猜，你每次可以猜一个字母。
如果单词里有那个字母，所有该字母会显示出来；
如果没有那个字母，则计算机会在一幅“刽子手”画上填一笔。
这幅画一共需要7笔就能完成，因此你最多只能错6次。
样例输入
cheese
abcdefg
样例输出
you lose
-}

module Main (main) where


main :: IO ()
main = do
    input <- getLine
    guest <- getLine
    let out = game (generateempty input , input , length input) guest 5
    putStrLn out
    

--匹配每次的字符
--(缓存字符串，匹配字符串，剩下可以正确匹配的字符数) -> 匹配的字符 -> (匹配是否成功，缓存的字符串，匹配的字符串，剩下可以正确匹配的字符数)
kill :: (String , String , Int) -> Char -> (Bool , String , String , Int)
kill ((a:as) , (b:bs) , g) pt = if b == pt
    then (True , (b:s) , ' ':e , guest-1)
    else (False || judge , (a:s) , b:e , guest)
    where (judge,s,e,guest) = kill (as,bs,g) pt
kill (_,_,g) _ = (False,[],[],g)

--开始游戏
--(s长度的空字符串，匹配字符串，s长度) -> 猜的字符 -> 可以猜的次数 -> 返回结果
game :: (String , String , Int) -> String -> Int -> String
game _ _ 0 = "you lose"
game (_,_,0) _ _ = "you win"
game (as,bs,guest) (c:cs) count = if j
    then s1 ++ "\n" ++ game (s1,s2,g) cs count
    else "once killed \n" ++ game (s1,s2,g) cs (count-1)
    where (j,s1,s2,g) = kill (as,bs,guest) c

--生成字符串s长度的‘ ’字符串
generateempty :: String -> String
generateempty s = estring $ length s
    where 
        estring 0 = []
        estring l = ' ' : estring (l-1)