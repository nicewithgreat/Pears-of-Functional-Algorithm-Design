import Data.Bits

countBits :: Int -> Int
countBits x | x == 0 = 0
            | otherwise = ((.&.) x 1) + (countBits (shiftR x 1))


-- {-# LANGUAGE CPP #-}
-- module BitCounting (countBits) where

--     import Data.Bits {- /* -} hiding {- */ -} (popCount, popCountDefault)
    
--     countBits :: Int -> Int
--     countBits = popCount
          