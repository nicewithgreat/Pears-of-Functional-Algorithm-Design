{-
In this kata you have to implement a base converter, which converts positive integers between arbitrary bases / alphabets. Here are some pre-defined alphabets:

newtype Alphabet = Alphabet { getDigits :: [Char] } deriving (Show)
bin, oct, dec, hex, alphaLower, alphaUpper, alpha, alphaNumeric :: Alphabet
bin = Alphabet $ "01"
oct = Alphabet $ ['0'..'7']
dec = Alphabet $ ['0'..'9']
hex = Alphabet $ ['0'..'9'] ++ ['a'..'f']
alphaLower    = Alphabet $ ['a'..'z']
alphaUpper    = Alphabet $ ['A'..'Z']
alpha         = Alphabet $ ['a'..'z'] ++ ['A'..'Z']
alphaNumeric  = Alphabet $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
The function convert() should take an input (string), the source alphabet (string) and the target alphabet (string). You can assume that the input value always consists of characters from the source alphabet. You don't need to validate it.

Examples
convert dec bin "15"   `shouldBe` "1111"
convert dec oct "15"   `shouldBe` "17"
convert bin dec "1010" `shouldBe` "10"
convert bin hex "1010" `shouldBe` "a"
convert dec alpha      "0"     `shouldBe` "a"
convert dec alphaLower "27"    `shouldBe` "bb"
convert alphaLower hex "hello" `shouldBe` "320048"
Additional Notes:

The maximum input value can always be encoded in a number without loss of precision in JavaScript. In Haskell, intermediate results will probably be too large for Int.
The function must work for any arbitrary alphabets, not only the pre-defined ones
You don't have to consider negative numbers
ALGORITHMSUTILITIESSTRINGSMATHEMATICSNUMBERS


-}
{-sample test
module BaseConversionSpec (spec, main) where
import BaseConversion (convert, Alphabet(..))
import BaseConversion.Normalize (normalize)
import Test.Hspec
import Test.QuickCheck

bin, oct, dec, hex, alphaLower, alphaUpper, alpha, alphaNumeric :: Alphabet
bin = Alphabet $ "01"
oct = Alphabet $ ['0'..'7']
dec = Alphabet $ ['0'..'9']
hex = Alphabet $ ['0'..'9'] ++ ['a'..'f']
alphaLower    = Alphabet $ ['a'..'z']
alphaUpper    = Alphabet $ ['A'..'Z']
alpha         = Alphabet $ ['a'..'z'] ++ ['A'..'Z']
alphaNumeric  = Alphabet $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

allAlphabets  = [bin, oct, dec, hex, alphaLower, alphaUpper, alpha, alphaNumeric]

main = hspec spec
spec = do
  describe "convert" $ do
    it "should work on some simple examples" $ do
      convert dec bin "15"   `shouldBe` "1111"
      convert dec oct "15"   `shouldBe` "17"
      convert bin dec "1010" `shouldBe` "10"
      convert bin hex "1010" `shouldBe` "a"
      
      convert dec alpha      "0"     `shouldBe` "a"
      convert dec alphaLower "27"    `shouldBe` "bb"
      convert alphaLower hex "hello" `shouldBe` "320048"
      
    it "shouldn't change anything for the same base" $ do
      property $ 
        forAll (elements allAlphabets) $ \a ->
        forAll (listOf1 . elements . getDigits $ a) $ \x ->
        let xs = normalize a x
        in convert a a xs `shouldBe` xs
        
    it "should be able to revert the conversion" $ do
      property $ 
        forAll (elements allAlphabets) $ \a ->
        forAll (elements allAlphabets) $ \b ->
        forAll (listOf1 . elements . getDigits $ a) $ \x ->
        let xs = normalize a x
        in convert b a (convert a b xs) `shouldBe` xs

-}

-- module BaseConversion where

newtype Alphabet = Alphabet { getDigits :: [Char] } deriving (Show)

bin, oct, dec, hex, alphaLower, alphaUpper, alpha, alphaNumeric :: Alphabet
bin = Alphabet $ "01"
oct = Alphabet $ ['0'..'7']
dec = Alphabet $ ['0'..'9']
hex = Alphabet $ ['0'..'9'] ++ ['a'..'f']
alphaLower    = Alphabet $ ['a'..'z']
alphaUpper    = Alphabet $ ['A'..'Z']
alpha         = Alphabet $ ['a'..'z'] ++ ['A'..'Z']
alphaNumeric  = Alphabet $ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

allAlphabets  = [bin, oct, dec, hex, alphaLower, alphaUpper, alpha, alphaNumeric]

convert :: Alphabet -> Alphabet -> String -> String
convert (Alphabet a) (Alphabet b) xs = bx
  where alength = intLength a
        blength = intLength b
        ax = [i|x<-xs,(i,v)<-(zipWith (,) [0..] a),x==v]
        bx = [v|x<-ft,(i,v)<-(zipWith (,) [0..] b),x==i]
        tt = toTen (reverse ax) alength 1
        ft = reverse (fromTen tt blength)

intLength :: [Char] -> Integer
intLength = foldr ((+).(\_->1)) 0 

        
fromTen :: Integer -> Integer -> [Integer]
fromTen m n | m>=n =(mod m n) : (fromTen (div m n) n)
            | otherwise = [m]


toTen :: [Integer]  -> Integer -> Integer -> Integer
toTen [] _ _ = 0
toTen (x:xs) n acc = acc * x + toTen xs n (acc*n)



-- convert :: Alphabet -> Alphabet -> String -> String
-- convert (Alphabet a) (Alphabet b) x = error "todo: convert"
--     where   a'= foldr ((:).find a ) [] x
--             alength = length a
--             blength = length b


-- cv :: Integer -> Integer -> [Integer] -> [Integer]
-- cv a b (x:xs)   | x>=b = --mod x b : cv a b xs
--                 | otherwise 
-- --foldr ((:).find ['a'..'d'] ) [] ['a'..'d']
-- find :: String -> Char -> Integer
-- find [] _ = -1
-- find (x:xs) x'  | x /= x' = 1 + find xs x'
--                 | otherwise = 0
{-zelixir
import Numeric
import Data.List
import Data.Maybe
convert :: Alphabet -> Alphabet -> String -> String
convert (Alphabet a) (Alphabet b) = flip (showIntAtBase (genericLength b) (b!!)) "" . fst. head . readInt (genericLength a) (const True) (fromJust . flip findIndex a . (==))
-}
