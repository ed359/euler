module Problem076 (main) where

-- It is possible to write five as a sum in exactly six different ways:

-- 4 + 1
-- 3 + 2
-- 3 + 1 + 1
-- 2 + 2 + 1
-- 2 + 1 + 1 + 1
-- 1 + 1 + 1 + 1 + 1

-- How many different ways can one hundred be written as a sum of at least two positive integers?

-- A memoized Euler formula should require O(n^2) multiplications.
-- Faster methods exist for generating the entire list.
-- Much faster methods (e.g. Rademacher's series) exist for one-shot partition n computation, but
-- they involve lots of fancy mathematics functions.
partition :: Int -> Int
partition 0 = 1
partition n = sum [(-1)^(k+1) * (partition (n-(k*(3*k+1) `div` 2)) + partition (n-(k*(3*k-1) `div` 2))) |
                   k <- takeWhile (\l -> (6*l-1)^2 <= 24*n+1) [1..]]

memoizedPartition :: Int -> Int
memoizedPartition n | n < 0 = 0
                    | otherwise = map p [0 ..] !! n
   where p 0 = 1
         p m = sum [(-1)^(k+1) * (memoizedPartition (m-(k*(3*k+1) `div` 2)) + memoizedPartition (m-(k*(3*k-1) `div` 2))) |
                   k <- takeWhile (\l -> (6*l-1)^2 <= 24*m+1) [1..]]

main :: IO ()
main = print $ memoizedPartition 100 - 1