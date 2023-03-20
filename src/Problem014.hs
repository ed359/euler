module Problem014 (main, solve) where

import qualified Data.Map as M
import Data.Foldable (foldl', maximumBy)
import Data.Ord (comparing)

next :: Integral a => a -> a
next n = if even n then n `div` 2 else 3*n + 1

collatzSequence :: Int -> [Int]
collatzSequence 1 = [1]
collatzSequence n = n : collatzSequence (next n)

naiveCollatzLength :: Int -> Int
naiveCollatzLength = length . collatzSequence

collatzLength :: Int -> Int
collatzLength 1 = 1
collatzLength n 
    | n' <= memBound = 1 + M.findWithDefault 0 n' memCollatzLength
    | otherwise      = 1 + collatzLength n'
    where n' = next n

memBound :: Int
memBound = 1000000
memCollatzLength :: M.Map Int Int
memCollatzLength = M.fromList [(n, collatzLength n) | n <- [1..memBound]]
    

-- takes about 0.5s
solve :: Int -> Int
solve n = maximumBy (comparing collatzLength) [1..n-1]

main :: IO ()
main = print $ solve 1000000