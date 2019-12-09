{-
You live in the city of Cartesia where all roads are laid out in a perfect grid. You arrived ten minutes too early to an appointment, so you decided to take the opportunity to go for a short walk. The city provides its citizens with a Walk Generating App on their phones -- everytime you press the button it sends you an array of one-letter strings representing directions to walk (eg. ['n', 's', 'w', 'e']). You always walk only a single block in a direction and you know it takes you one minute to traverse one city block, so create a function that will return true if the walk the app gives you will take you exactly ten minutes (you don't want to be early or late!) and will, of course, return you to your starting point. Return false otherwise.

Note: you will always receive a valid array containing a random assortment of direction letters ('n', 's', 'e', or 'w' only). It will never give you an empty array (that's not a walk, that's standing still!).
-}
{-Sample Tests
module Codewars.Kata.TenMinuteWalk.Test where
import Codewars.Kata.TenMinuteWalk (isValidWalk)
import Control.Monad (when)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "isValidWalk" $ do
    it "should work for some examples" $ do
      isValidWalk ['n','s','n','s','n','s','n','s','n','s'] ??? "should return True  on valid walk"
      isValidWalk ['n','s','n','s','n','s','n','s','n','n'] ??! "should return False on invalid walk"
      isValidWalk ['n','s']    ??! "should return False on too short walk"
      isValidWalk (repeat 'n') ??! "should return False on infinite walk"
      isValidWalk ['n','s','e','w','n','s','e','w','n','s'] ??? "should return True on valid walk"
    it "should reject short walks" $ do
      property $ 
        forAll (choose (1,9)) $ \n ->
        forAll (listOf1 $ elements "nswe") $ \xs ->
          let walk = take n xs 
          in isValidWalk walk ??! "the walk \""++walk++"\" is too short and should be rejected"
    it "should work for semi-random valid walks" $ do
      property $ 
        forAll (choose (0,3)) $ \n ->
          let k = 5 - n
              w = replicate n 's' ++ replicate n 'n' ++ replicate k 'w' ++ replicate k 'e'
          in isValidWalk w ??? "the walk \""++w++"\" is valid short and should be accepted"
    it "should reject infinite lists" $ do
      property $ forAll infiniteList $ \walk ->
        isValidWalk walk ??! "this infinite walk should have been rejected"
      
-- | Additional helpers to provide better error messages
--   on boolean functions.
(???), (??!) :: Bool -> String -> Expectation
(??!) p = when p       . expectationFailure
(???) p = when (not p) . expectationFailure

infix 0 ??!, ???

-}

{-
module Codewars.Kata.TenMinuteWalk where
-}
isValidWalk :: [Char] -> Bool
isValidWalk walk    | length(take 11 walk) /= 10 = False
                    | otherwise = 0 == (change walk)

north = 1
south = -1
east = -0.1
west = 0.1

change [] = 0
change (x:xs)   | x == 'e' = -10 + change xs
                | x == 's' = -1  + change xs
                | x == 'w' = 10  + change xs
                | x == 'n' = 1   + change xs