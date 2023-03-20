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
collatzLength n = fst $ memCollatzLength n M.empty

memCollatzLength :: Int -> M.Map Int Int -> (Int, M.Map Int Int)
memCollatzLength 1 m = (1, m)
memCollatzLength n m = case M.lookup n m of 
    Just l -> (l, m)
    Nothing -> (nl + 1, M.insert n (nl+1) (M.union nm m))
  where
    (nl, nm) = memCollatzLength (next n) m

-- takes about 10s
solve :: Int -> Int
solve n = maximumBy (comparing collatzLength) [1..n-1]

main :: IO ()
main = print $ solve 1000000