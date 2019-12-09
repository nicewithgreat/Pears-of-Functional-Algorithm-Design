{-
Description:
Given an array (arr) as an argument complete the function countSmileys that should return the total number of smiling faces.
Rules for a smiling face:
-Each smiley face must contain a valid pair of eyes. Eyes can be marked as : or ;
-A smiley face can have a nose but it does not have to. Valid characters for a nose are - or ~
-Every smiling face must have a smiling mouth that should be marked with either ) or D.
No additional characters are allowed except for those mentioned.
Valid smiley face examples:
:) :D ;-D :~)
Invalid smiley faces:
;( :> :} :]

Example cases:

countSmileys([':)', ';(', ';}', ':-D']);       // should return 2;
countSmileys([';D', ':-(', ':-)', ';~)']);     // should return 3;
countSmileys([';]', ':[', ';*', ':$', ';-D']); // should return 1;

Note: In case of an empty array return 0. You will not be tested with invalid input (input will always be an array). Order of the face (eyes, nose, mouth) elements will always be the same
-}
import Data.List


eyes = [':',';']
nose = ['-','~']
mouth = [')','D']

smileyFace :: String -> Bool
smileyFace (x:xs@(y:ys)) =  if elem x eyes 
                            then    if null ys 
                                    then elem y mouth
                                    else elem y nose && elem (head ys) mouth
                            else False
smileyFace _ = False                             

countSmileys :: [String] -> Int
countSmileys = foldr (\x -> if smileyFace x then (+1) else (+0)) 0

(+++) s1 s2 s3 = s1 ++ s2 ++ s3

{-
(+++) s1 s2 s3 = s1 ++ s2 ++ s3

valids = (+++) <$> [";", ":"] <*> ["~", "-", ""] <*> ["D", ")"]

isValid = (`elem` valids)

countSmileys :: [String] -> Int
countSmileys = length . filter isValid
-}

{-
lumie1337's answer:

module Smile where

newtype Parser a = Parser { parse :: String -> [(a,String)] }

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return a = Parser (\s -> [(a,s)])
  (>>=) p f  = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

runParser :: Parser a -> String -> Bool
runParser m s =
  case parse m s of
    [(res, [])] -> True
    [(_, rs)]   -> False
    _           -> False

parseChar :: [Char] -> Parser Char
parseChar cs = Parser $ \s -> 
  case s of
    (c:s') | c `elem` cs -> [(c, s')]
    _                    -> []

parseCharOpt :: [Char] -> Parser (Maybe Char)
parseCharOpt cs = Parser $ \s -> 
  case s of
    (c:s') | c `elem` cs -> [(Just c, s')]
    _                    -> [(Nothing, s)]

parseEye  = parseChar [':', ';']
parseNose = parseCharOpt ['-', '~']
parseSmiley = parseChar [')', 'D']

isSmiley s = 
  runParser (parseEye >> parseNose >> parseSmiley) s

countSmileys :: [String] -> Int
countSmileys = length . filter isSmiley
-}