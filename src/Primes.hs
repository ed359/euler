module Primes (
  sieve,
  primes,
  primesStr,
  primeFactors,
  groupFactors,
  primeFactorsGroup,
  combineFactorsGroup,
  countDivisors',
  numDivisors,
  numDivisorsProduct,
  numDivisorsProductList,
  divisorsFromFactorsGroup,
  divisors,
  divisorsProduct,
  divisorsProductList,
  divisorsInOrder,
  divisorsInOrder',
  totient,
) where

import Data.List (foldl', group)
import Data.Map.Strict qualified as M
import Data.Stream (Stream (Cons), (<:>))
import Data.Stream qualified as Str

primes :: Integral a => [a]
primes = sieve [2 ..]
{-# SPECIALIZE primes :: [Int] #-}
{-# SPECIALIZE primes :: [Integer] #-}

-- https://doi.org/10.1017/S0956796808007004
sieve :: Integral a => [a] -> [a]
sieve xs = sieve' xs M.empty
 where
  sieve' [] _ = []
  sieve' (y : ys) table = case M.lookup y table of
    Nothing -> y : sieve' ys (M.insert (y * y) [y] table)
    Just facts -> sieve' ys (foldl reinsert (M.delete y table) facts)
   where
    reinsert table' prime = M.insertWith (++) (y + prime) [prime] table'
{-# SPECIALIZE sieve :: [Int] -> [Int] #-}
{-# SPECIALIZE sieve :: [Integer] -> [Integer] #-}

-- Slightly neater version which makes us of the fact that there are infinitely many primes
primesStr :: Integral a => Stream a
primesStr = sieve' (Str.fromList [2 ..]) M.empty
 where
  sieve' (Cons y ys) table = case M.lookup y table of
    Nothing -> y <:> sieve' ys (M.insert (y * y) [y] table)
    Just facts -> sieve' ys (foldl reinsert (M.delete y table) facts)
   where
    reinsert table' prime = M.insertWith (++) (y + prime) [prime] table'
{-# SPECIALIZE primesStr :: Stream Int #-}
{-# SPECIALIZE primesStr :: Stream Integer #-}

-- Trial division by primes
primeFactors :: Integral a => a -> [a]
primeFactors = go primesStr
 where
  go _ 1 = []
  go ps@(Cons p pps) m
    | m < p * p = [m]
    | r == 0 = p : go ps q
    | otherwise = go pps m
   where
    (q, r) = quotRem m p
{-# SPECIALIZE primeFactors :: Int -> [Int] #-}
{-# SPECIALIZE primeFactors :: Integer -> [Integer] #-}

groupFactors :: (Integral a, Integral b) => [a] -> [(a, b)]
groupFactors = map (\xs -> if null xs then (1, 1) else (head xs, fromIntegral (length xs))) . group
{-# SPECIALIZE groupFactors :: [Int] -> [(Int, Int)] #-}
{-# SPECIALIZE groupFactors :: [Integer] -> [(Integer, Int)] #-}
{-# SPECIALIZE groupFactors :: [Integer] -> [(Integer, Integer)] #-}

-- List of (prime, exponent) pairs
primeFactorsGroup :: (Integral a, Integral b) => a -> [(a, b)]
primeFactorsGroup = groupFactors . primeFactors
{-# SPECIALIZE primeFactorsGroup :: Int -> [(Int, Int)] #-}
{-# SPECIALIZE primeFactorsGroup :: Integer -> [(Integer, Int)] #-}
{-# SPECIALIZE primeFactorsGroup :: Integer -> [(Integer, Integer)] #-}

-- | Compute the grouped prime factors of a product m*n from the individual grouped prime factors
combineFactorsGroup :: (Ord a, Num b) => [(a, b)] -> [(a, b)] -> [(a, b)]
combineFactorsGroup [] ys = ys
combineFactorsGroup xs [] = xs
combineFactorsGroup xs@((p, e) : xss) ys@((q, f) : yss)
  | p < q = (p, e) : combineFactorsGroup xss ys
  | p > q = (q, f) : combineFactorsGroup xs yss
  | p == q = (p, e + f) : combineFactorsGroup xss yss

primeFactorsGroupProduct :: (Integral a, Integral b) => a -> a -> [(a, b)]
primeFactorsGroupProduct m n = combineFactorsGroup (primeFactorsGroup m) (primeFactorsGroup n)

-- | Helper function for counting divisors
countDivisors' :: Integral b => [(a, b)] -> b
countDivisors' = foldl' (\r (_, e) -> r * (e + 1)) 1

numDivisors :: (Integral a, Integral b) => a -> b
numDivisors n = countDivisors' $ primeFactorsGroup n
{-# SPECIALIZE numDivisors :: Int -> Int #-}
{-# SPECIALIZE numDivisors :: Integer -> Int #-}
{-# SPECIALIZE numDivisors :: Integer -> Integer #-}

-- | Optimized numDivisors in the case of a product m*n
numDivisorsProduct :: (Integral a, Integral b) => a -> a -> b
numDivisorsProduct m n = countDivisors' $ combineFactorsGroup (primeFactorsGroup m) (primeFactorsGroup n)
{-# SPECIALIZE numDivisorsProduct :: Int -> Int -> Int #-}
{-# SPECIALIZE numDivisorsProduct :: Integer -> Integer -> Int #-}
{-# SPECIALIZE numDivisorsProduct :: Integer -> Integer -> Integer #-}

numDivisorsProductList :: (Integral a, Integral b) => [a] -> b
numDivisorsProductList ns = countDivisors' $ foldl' combineFactorsGroup [] $ map primeFactorsGroup ns
{-# SPECIALIZE numDivisorsProductList :: [Int] -> Int #-}
{-# SPECIALIZE numDivisorsProductList :: [Integer] -> Int #-}
{-# SPECIALIZE numDivisorsProductList :: [Integer] -> Integer #-}

-- All the divisors of a number (including 1 and itself)
divisors :: Integral a => a -> [a]
divisors n = product <$> mapM (\(p, e) -> [p ^ k | k <- [0 .. e]]) (primeFactorsGroup n)
{-# SPECIALIZE divisors :: Int -> [Int] #-}
{-# SPECIALIZE divisors :: Integer -> [Integer] #-}

divisorsInOrder :: Integral a => a -> [a]
divisorsInOrder n = foldr (\(p, e) ds -> concatMap (\d -> [d * p ^ k | k <- [0 .. e]]) ds) [1] $ primeFactorsGroup n
{-# SPECIALIZE divisorsInOrder :: Int -> [Int] #-}
{-# SPECIALIZE divisorsInOrder :: Integer -> [Integer] #-}

divisorsFromFactorsGroup :: (Foldable t, Num a) => t (a, Int) -> [a]
divisorsFromFactorsGroup = foldl' (\ds (p, e) -> concatMap (\d -> [d * p ^ k | k <- [0 .. e]]) ds) [1]

divisorsInOrder' :: Integral a => a -> [a]
divisorsInOrder' n = divisorsFromFactorsGroup $ primeFactorsGroup n
{-# SPECIALIZE divisorsInOrder' :: Int -> [Int] #-}
{-# SPECIALIZE divisorsInOrder' :: Integer -> [Integer] #-}

divisorsProduct :: Integral a => a -> a -> [a]
divisorsProduct m n = divisorsFromFactorsGroup $ combineFactorsGroup (primeFactorsGroup m) (primeFactorsGroup n)
{-# SPECIALIZE divisorsProduct :: Int -> Int -> [Int] #-}
{-# SPECIALIZE divisorsProduct :: Integer -> Integer -> [Integer] #-}

divisorsProductList :: Integral a => [a] -> [a]
divisorsProductList ns = divisorsFromFactorsGroup $ foldl' combineFactorsGroup [] $ map primeFactorsGroup ns
{-# SPECIALIZE divisorsProductList :: [Int] -> [Int] #-}
{-# SPECIALIZE divisorsProductList :: [Integer] -> [Integer] #-}

-- Euler totient
totient :: Integral a => a -> a
totient n = foldl' (\a (p, e) -> a * p ^ (e - 1) * (p - 1)) 1 $ primeFactorsGroup n
{-# SPECIALIZE totient :: Int -> Int #-}
{-# SPECIALIZE totient :: Integer -> Integer #-}