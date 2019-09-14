qSrot ::(Ord a)=>[a]->[a]
qSort []=[]
qSort (x:xs)=qSrot l ++ [x] ++qSort r
    where (l,r)=partition (<x) xs