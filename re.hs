{-
The goal of this exercise is to convert a string to a new string where each character in the new string is "(" if that character appears only once in the original string, or ")" if that character appears more than once in the original string. Ignore capitalization when determining if a character is a duplicate.

Examples
"din"      =>  "((("
"recede"   =>  "()()()"
"Success"  =>  ")())())"
"(( @"     =>  "))((" 
Notes

Assertion messages may be unclear about what they display in some languages. If you read "...It Should encode XXX", the "XXX" is the expected result, not the input!
-}
import Data.Char

duplicateEncode :: String -> String
duplicateEncode sl = deal2 s n
  where (n,_) = deal s ([],[])
        s = map toUpper sl

deal2 :: String -> String -> String
deal2 [] _ = []
deal2 (a:ax) n  | elem a n = '(' : deal2 ax n
                | otherwise = ')' : deal2 ax n
                
deal :: String -> (String,String) -> (String,String)
deal [] r = r
deal (a:ax) (n,y) | (not $ elem a y) && (elem a n) = deal ax (n',a:y)
                  | (not $ elem a y) && (not $ elem a n)= deal ax (a:n,y)
                  | otherwise = deal ax (n,y)
                  where n' = [x | x <- n , x /= a]
{-
--other answer,it so concise
module Dups where

import Data.Char

duplicateEncode :: String -> String
duplicateEncode xs = map encode xs' where
  xs' = map toLower xs
  encode c = if length (filter (== c) xs') > 1 then ')' else '('

-}