module Problem077 (main, primePartitions, naivePrimePartitions) where

-- It is possible to write ten as the sum of primes in exactly five different ways:

-- 7 + 3
-- 5 + 5
-- 5 + 3 + 2
-- 3 + 3 + 2 + 2
-- 2 + 2 + 2 + 2 + 2

-- What is the first value which can be written as the sum of primes in over five thousand different ways?

import Primes (primes)
import qualified Data.Set as S
import qualified Data.SortedList as SL

naivePrimePartitions :: Int -> S.Set (SL.SortedList Int)
naivePrimePartitions 0 = S.singleton mempty
naivePrimePartitions 1 = S.empty
naivePrimePartitions n = S.fromList [p `SL.insert` ps | p <- takeWhile (<= n) primes, ps <- S.toList $ naivePrimePartitions (n-p)]

primePartitions :: Int -> S.Set (SL.SortedList Int)
primePartitions n = map f [0..] !! n
    where 
        f :: Int -> S.Set (SL.SortedList Int)
        f 0 = S.singleton mempty
        f 1 = S.empty
        f n = S.fromList [p `SL.insert` ps | p <- takeWhile (<= n) primes, ps <- S.toList $ primePartitions (n-p)]

solve :: Int -> Int
solve n = fst . head $ dropWhile (\(_, pps) -> S.size pps <= n) (map (\m -> (m, primePartitions m)) [2..])

main :: IO ()
main = print $ solve 5000