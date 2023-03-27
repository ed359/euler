module Problem036 (main) where

-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

-- (Please note that the palindromic number, in either base, may not include leading zeros.)

isPalindrome :: Integral a => a -> a -> Bool
isPalindrome base n = n == reverseDigits base n

reverseDigits :: Integral a => a -> a -> a
reverseDigits base n = go n 0 len
  where
    len = length $ takeWhile (> 0) $ iterate (`quot` base) n
    go _ n' 0 = n'
    go n n' l = let (q, r) = n `quotRem` base in go q (n' + r * base ^ (l - 1)) (l - 1)

solve :: Integral a => a -> a
solve n = sum [x | x <- [1 .. n - 1], isPalindrome 2 x && isPalindrome 10 x]

-- This is quite slow unless optimizations are applied.
-- GHC's interpreter: ~30s
-- GHC no flags:       ~8s
-- GHC -O1             <0.5s 
-- GHC -O2             <0.5s 
main :: IO ()
main = print $ solve (1_000_000 :: Int)