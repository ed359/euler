module Primes (primes, sieve, primeFactors, primeFactorsGroup, divisors, totient) where

import Data.List (foldl', group)
import Data.Map.Strict qualified as M

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

-- Trial division by primes
primeFactors :: Integral a => a -> [a]
primeFactors n = go n primes
 where
  go 1 _ = []
  go _ [] = undefined -- unreachable as primes is nonempty
  go m (p : ps)
    | m < p * p = [m]
    | r == 0 = p : go q (p : ps)
    | otherwise = go m ps
   where
    (q, r) = quotRem m p
{-# SPECIALIZE primeFactors :: Int -> [Int] #-}
{-# SPECIALIZE primeFactors :: Integer -> [Integer] #-}

-- List of (prime, exponent) pairs
primeFactorsGroup :: Integral a => a -> [(a, Int)]
primeFactorsGroup n = map (\xs -> (head xs, length xs)) $ Data.List.group $ primeFactors n
{-# SPECIALIZE primeFactorsGroup :: Int -> [(Int, Int)] #-}
{-# SPECIALIZE primeFactorsGroup :: Integer -> [(Integer, Int)] #-}

-- All the divisors of a number (including 1 and itself)
divisors :: Integral a => a -> [a]
divisors n = product <$> mapM (\(p, e) -> [p ^ k | k <- [0 .. e]]) (primeFactorsGroup n)
{-# SPECIALIZE divisors :: Int -> [Int] #-}
{-# SPECIALIZE divisors :: Integer -> [Integer] #-}

divisorsInOrder :: Integral a => a -> [a]
divisorsInOrder n = foldr (\(p, e) ds -> concatMap (\d -> [d * p ^ k | k <- [0 .. e]]) ds) [1] $ primeFactorsGroup n
{-# SPECIALIZE divisorsInOrder :: Int -> [Int] #-}
{-# SPECIALIZE divisorsInOrder :: Integer -> [Integer] #-}

divisorsInOrder' :: Integral a => a -> [a]
divisorsInOrder' n = foldl' (\ds (p, e) -> concatMap (\d -> [d * p ^ k | k <- [0 .. e]]) ds) [1] $ primeFactorsGroup n
{-# SPECIALIZE divisorsInOrder' :: Int -> [Int] #-}
{-# SPECIALIZE divisorsInOrder' :: Integer -> [Integer] #-}

-- Euler totient
totient :: Integral a => a -> a
totient n = foldl' (\a (p, e) -> a * p ^ (e - 1) * (p - 1)) 1 $ primeFactorsGroup n
{-# SPECIALIZE totient :: Int -> Int #-}
{-# SPECIALIZE totient :: Integer -> Integer #-}