module Problem014 (main, solve) where

import Data.Array (Array, Ix, array, inRange, range, (!))
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

next :: Integral a => a -> a
next n = if even n then n `div` 2 else 3*n + 1

collatzSequence :: Int -> [Int]
collatzSequence 1 = [1]
collatzSequence n = n : collatzSequence (next n)

naiveCollatzLength :: Int -> Int
naiveCollatzLength = length . collatzSequence

collatzLengthAbstract :: (Int -> Int) -> Int -> Int
collatzLengthAbstract _ 1 = 1
collatzLengthAbstract r n
    | even n = 1 + r (n `div` 2)
    | otherwise = 1 + r (3*n + 1)

buildMemo :: (Ix i) => (i, i) -> (i -> e) -> Array i e
buildMemo bounds f = array bounds $ map (\x -> (x, f x)) $ range bounds

memoRange :: (Ix a) => (a, a) -> ((a -> b) -> a -> b) -> a -> b
memoRange bounds abstractF = arrayLookup
    where
        memoArray = buildMemo bounds (abstractF arrayLookup)
        arrayLookup x = if inRange bounds x then memoArray ! x else abstractF arrayLookup x

collatzLengthMemo :: Int -> Int -> Int
collatzLengthMemo n = memoRange (1, n) collatzLengthAbstract

-- takes about 0.1s
solve :: Int -> Int
solve n = let f = collatzLengthMemo n in maximumBy (comparing f) [1..n-1]

main :: IO ()
main = print $ solve 1000000