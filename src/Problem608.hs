module Problem608 (main) where

-- Let D(m,n)=\sum_{d|m}\sum_{k=1}^n\sigma_{0}(kd) where d runs through all divisors of m and \sigma_{0}(n)
-- is the number of divisors of $n$.

-- You are given D(3!,10^2)=3398 and D(4!,10^6)=268882292.

-- Find D(200!,10^{12}) mod (10^9 + 7)

import Control.Monad (when)
import Data.Array qualified as A
import Data.Array.IArray (Array, IArray, Ix, inRange, range)
import Data.Array.IArray qualified as IA
import Data.Array.MArray qualified as MA
import Data.Array.ST (MArray, runSTArray)
import Data.Foldable (Foldable (foldl'), forM_)
import Data.Functor.Identity (runIdentity)
import Data.List (group)
import Data.Map.Strict qualified as M
import Data.Stream (Stream (Cons), (<:>))
import Data.Stream qualified as Stream

import PrimesInt (combineFactorsGroup, countDivisors', divisorsFromFactorsGroup, groupFactors, primes)

-- 1. Build a memoized array of the form array [(i, primeFactorsGroup i) | i <- [1..]]
primeFactorsAbstract :: (Applicative f) => (Stream Int -> Int -> f [Int]) -> Stream Int -> Int -> f [Int]
primeFactorsAbstract g _ 1 = pure []
primeFactorsAbstract g ps@(Cons p pps) m
  | m < p * p = pure [m]
  | r == 0 = (p :) <$> g ps q
  | otherwise = g pps m
 where
  (q, r) = quotRem m p

primeFactorsFunc :: Int -> [Int]
primeFactorsFunc = runIdentity . go primes
 where
  go = primeFactorsAbstract go

buildPrimeFactorsArray :: Int -> Array Int [Int]
buildPrimeFactorsArray n = runSTArray $ do
  stArr <- MA.newArray memBounds nil
  MA.writeArray stArr 1 []
  forM_ (range memBounds) $ \j -> do
    ps <- mutableLookup primeFactorsAbstract nil stArr primes j
    MA.writeArray stArr j ps
  return stArr
 where
  memBounds = (1, n)
  nil = [-1]
  mutableLookup applicativeF nil arr qs j = do
    memBounds <- MA.getBounds arr
    if inRange memBounds j
      then do
        x <- MA.readArray arr j
        when (x == nil) $ do
          y <- applicativeF go qs j -- recurse if the value for j hasn't been computed yet
          MA.writeArray arr j y
        MA.readArray arr j
      else applicativeF go qs j -- recurse if j is out of bounds
   where
    go = mutableLookup applicativeF nil arr

buildPrimeFactorsGroupArray :: Int -> Array Int [(Int, Int)]
buildPrimeFactorsGroupArray n = groupFactors <$> buildPrimeFactorsArray n

-- maxMemo :: Int
-- maxMemo = 1_000

-- primeFactorsArray :: Array Int [Int]
-- primeFactorsArray = buildPrimeFactorsArray maxMemo

-- primeFactorsGroupArray :: Array Int [(Int, Int)]
-- primeFactorsGroupArray = buildPrimeFactorsGroupArray maxMemo

-- TODO: reader monad
primeFactors :: Array Int [Int] -> Int -> [Int]
primeFactors a n = if inRange (IA.bounds a) n then a IA.! n else primeFactorsFunc n

primeFactorsGroup :: Array Int [(Int, Int)] -> Int -> [(Int, Int)]
primeFactorsGroup a n = if inRange (IA.bounds a) n then a IA.! n else groupFactors $ primeFactorsFunc n

numDivisors :: Array Int [(Int, Int)] -> Int -> Int
numDivisors a n = countDivisors' $ primeFactorsGroup a n

numDivisorsProduct :: Array Int [(Int, Int)] -> Int -> Int -> Int
numDivisorsProduct a m n = countDivisors' $ combineFactorsGroup (primeFactorsGroup a m) (primeFactorsGroup a n)

divisors :: Array Int [(Int, Int)] ->  Int -> [Int]
divisors a n = product <$> mapM (\(p, e) -> [p ^ k | k <- [0 .. e]]) (primeFactorsGroup a n)

divisorsProduct :: Array Int [(Int, Int)] -> Int -> Int -> [Int]
divisorsProduct a m n = divisorsFromFactorsGroup $ combineFactorsGroup (primeFactorsGroup a m) (primeFactorsGroup a n)

divisorsProductList :: Array Int [(Int, Int)] -> [Int] -> [Int]
divisorsProductList a ns = divisorsFromFactorsGroup $ foldl' combineFactorsGroup [] $ map (primeFactorsGroup a) ns

-- 2. Implement the D function

-- -- Naive implementation is slow.
-- dfunc1 :: Int -> Int -> Int
-- dfunc1 m n = sum [sum [numDivisors (k * d) | k <- [1 .. n]] | d <- divisors m]

-- -- Optimize the number of divisors of a product
-- dfunc2 :: Int -> Int -> Int
-- dfunc2 m n = sum [sum [numDivisorsProduct k d | k <- [1 .. n]] | d <- divisors m]

-- -- Optimize the divisors of a product
-- dfunc3 :: [Int] -> Int -> Int
-- dfunc3 ms n = sum [sum [numDivisorsProduct k d | k <- [1 .. n]] | d <- divisorsProductList ms]

-- Recursion
dfunc4 :: Array Int [(Int, Int)] ->  [Int] -> Int -> Int
dfunc4 a ms 1 = sum [numDivisors a d | d <- divisorsProductList a ms]
dfunc4 a ms n = dfunc4 a ms (n-1) + sum [numDivisorsProduct a n d | d <- divisorsProductList a ms]

-- Takes about 3s
main :: IO ()
main = do
  let maxMemo = 10^6
  let a = buildPrimeFactorsGroupArray maxMemo
  print $ dfunc4 a [2..3] (10^2)
  print $ dfunc4 a [2..4] (10^6)
