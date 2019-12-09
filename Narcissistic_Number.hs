-- main = hspec $ do
--     describe "narcissistic" $ do
--       it "should work for some examples" $ do
--         narcissistic 7    `shouldBe` True
--         narcissistic 12   `shouldBe` False
--         narcissistic 370  `shouldBe` True
--         narcissistic 371  `shouldBe` True
--         narcissistic 1634 `shouldBe` True

-- narcissistic :: Integral n => n -> Bool
-- narcissistic n = True--getpower n
{-https://www.codewars.com/kata/5287e858c6b5a9678200083c/solutions/haskell-}

getpower m  | l /= 0 = r : getpower l
            | otherwise = [r]
    where   l = div m 10
            r = mod m 10

asum n a b = a + mor b n
    where   mor a 0 = 1
            mor a b = a * mor a (b-1)

narcissistic :: Integral n => n -> Bool
narcissistic x = result == x
    where   result = foldl (asum (length xs)) 0 xs
            xs = getpower x
{-
narcissistic :: Integral n => n -> Bool
narcissistic n = n == sum (map (^length digits) digits) where 
  digits = (map (`mod`10) . takeWhile (>0) . iterate (`div`10)) n
-}