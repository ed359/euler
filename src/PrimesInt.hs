module PrimesInt (
  primes,
  primesL,
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
  totient,
) where

import Data.List (foldl', group)
import Data.Map.Strict qualified as M
import Data.Stream (Stream (Cons), (<:>))
import Data.Stream qualified as Str
-- import Control.Monad (forM_, when)
-- import Control.Monad.ST (ST)
-- import Data.Array.IArray (IArray)
import Data.Array.IArray qualified as IA
-- import Data.Array.MArray (MArray)
-- import Data.Array.MArray qualified as MA
-- import Data.Array.ST (STArray, STUArray, runSTArray, runSTUArray)
import Data.Array.Unboxed (UArray)
-- import Data.Ix (Ix)
-- import Data.Ix qualified as Ix

import Primes (combineFactorsGroup, countDivisors', divisorsFromFactorsGroup, groupFactors)

primes :: Stream Int
primes = 2 <:> oddprimes ()
 where
  oddprimes = (3 <:>) . sieve' 3 [] . oddprimes
  sieve' x fs (Cons p ps) = Str.prefix [i * 2 + x | (i, True) <- IA.assocs a] $ sieve' (p * p) ((p, 0) : [(s, rem (y - q) s) | (s, y) <- fs]) ps
   where
    q = (p * p - x) `div` 2
    a :: UArray Int Bool
    a = IA.accumArray (\b c -> False) True (1, q - 1) [(i, ()) | (s, y) <- fs, i <- [y + s, y + s + s .. q]]

primesL :: [Int]
primesL = Str.toList primes

primeFactors :: Int -> [Int]
primeFactors = go primes
 where
  go _ 1 = []
  go ps@(Cons p pps) m
    | m < p * p = [m]
    | r == 0 = p : go ps q
    | otherwise = go pps m
   where
    (q, r) = quotRem m p

-- Memoize primeFactors, sees tricky as we recurse on both m and primes
primeFactorsAbstractC :: (Applicative f) => (Stream Int -> Int -> f [Int]) -> Stream Int -> Int -> f [Int]
primeFactorsAbstractC g _ 1 = pure []
primeFactorsAbstractC g ps@(Cons p pps) m
  | m < p * p = pure [m]
  | r == 0 = (p :) <$> g ps q
  | otherwise = g pps m
 where
  (q, r) = quotRem m p

-- List of (prime, exponent) pairs
primeFactorsGroup :: Int -> [(Int, Int)]
primeFactorsGroup = groupFactors . primeFactors

primeFactorsGroupProduct :: Int -> Int -> [(Int, Int)]
primeFactorsGroupProduct m n = combineFactorsGroup (primeFactorsGroup m) (primeFactorsGroup n)

numDivisors :: Int -> Int
numDivisors n = countDivisors' $ primeFactorsGroup n

-- | Optimized numDivisors in the case of a product m*n
numDivisorsProduct :: Int -> Int -> Int
numDivisorsProduct m n = countDivisors' $ combineFactorsGroup (primeFactorsGroup m) (primeFactorsGroup n)

numDivisorsProductList :: [Int] -> Int
numDivisorsProductList ns = countDivisors' $ foldl' combineFactorsGroup [] $ map primeFactorsGroup ns

-- All the divisors of a number (including 1 and itself)
divisors :: Int -> [Int]
divisors n = product <$> mapM (\(p, e) -> [p ^ k | k <- [0 .. e]]) (primeFactorsGroup n)

divisorsProduct :: Int -> Int -> [Int]
divisorsProduct m n = divisorsFromFactorsGroup $ combineFactorsGroup (primeFactorsGroup m) (primeFactorsGroup n)

divisorsProductList :: [Int] -> [Int]
divisorsProductList ns = divisorsFromFactorsGroup $ foldl' combineFactorsGroup [] $ map primeFactorsGroup ns

-- Euler totient
totient :: Int -> Int
totient n = foldl' (\a (p, e) -> a * p ^ (e - 1) * (p - 1)) 1 $ primeFactorsGroup n
