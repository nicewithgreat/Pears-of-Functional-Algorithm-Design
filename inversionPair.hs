module Main (main) where
--逆序对

main :: IO ()
main = return ()


qSort ::(Ord a)=>[a]->[a]
qSort []=[]
qSort (x:xs)= qSort l ++ [x] ++ qSort r
    where (l,r,count) = partition (<x) xs

partition :: (Ord a) => (a -> Bool) -> [a] -> ([a],[a],Int)
partition _ [] = (,,) [] [] 0
partition f (a:ax) =    if f a
                        then (a:l,r,count)
                        else (l,a:r,count+1)
                        where (l,r,count) = partition f ax
                        