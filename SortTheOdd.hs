-- You have an array of numbers.
-- Your task is to sort ascending odd numbers but even numbers must be on their places.

-- Zero isn't an odd number and you don't need to move it. If you have an empty array, you need to return it.

-- Example
--sortArray [5, 3, 2, 8, 1, 4] == [1, 3, 2, 8, 5, 4]
-- import Data.List

-- sortArray :: [Int] -> [Int]
-- sortArray xs = replace xs sortoxs
--     where   odds = [x|x<-xs,odd x]
--             sortoxs = qSort odds

-- replace :: [Int] -> [Int] -> [Int]
-- replace ax [] = ax       
-- replace (a:ax) (b:bx)   | odd a = b : replace ax bx
--                         | otherwise = a : replace ax (b:bx)

-- qSort ::(Ord a)=>[a]->[a]
-- qSort []=[]
-- qSort (x:xs)=qSort l ++ [x] ++qSort r
--     where (l,r)=partition (<x) xs

import Data.List (sort)

sortArray :: [Int] -> [Int]
sortArray = replaceOdd <$> id <*> sort . filter odd
  where replaceOdd xs [] = xs
        replaceOdd (x:xs) oos@(o:os)
          | even x    = x : replaceOdd xs oos
          | otherwise = o : replaceOdd xs os