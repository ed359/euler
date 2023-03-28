module Problem608 (main, bmain) where

-- Let D(m,n)=\sum_{d|m}\sum_{k=1}^n\sigma_{0}(kd) where d runs through all divisors of m and \sigma_{0}(n)
-- is the number of divisors of $n$.

-- You are given D(3!,10^2)=3398 and D(4!,10^6)=268882292.

-- Find D(200!,10^{12}) mod (10^9 + 7)\

-- Note that 200! has 139503973313460993785856000000 divisors, so we can't afford to iterate over them

-- Identity: \sum_{k=1}^n \sigma_0(k) = \sum_{k=1}^n \floor{n/k}
-- Proof: LHS = \sum_{k=1}^n \sum_{q | k} 1
--            = \sum_{k=1}^n\sum_{1 <= q, r <= n : qr = k} 1         via k = qr
--            = \sum_{1 <= q, r <= n : qr <= n} 1
--            = \sum_{r=1}^n \sum_{q = 1}^{\floor{n/r}} 1
--            = \sum_{r=1}^n \floor{n/r}

-- Identity: \sum_{k=1}^n \sigma_0(k) = 2 sum_{r = 1}^{isqrt x}(floor{n/r} - r)  + isqrt(n)
-- Proof: LHS = \sum_{1 <= q, r <= n : qr <= n} 1
--            = 2\sum_{1 <= q <= isqrt(n)}\sum_{q <= r <= n : qr <= n} 1
--            = 2\sum_{1 <= q <= isqrt(n)}\sum_{q <= r <= floor{n/q}} 1  + isqrt(n)
--            = 2\sum_{1 <= q <= isqrt(n)}(floor{n/q} - q)  + isqrt(n)

-- Indentity: \sum_{k=1}^n \sigma_0(kd) = \sum_{r=1}^{dn} \floor{dn/r}
-- Proof: LHS = \sum_{k=1}^n \sum_{q | kd} 1
--            = \sum_{k=1}^n \sum_{1 <= q, r <= kd : qr == kd} 1
--            = \sum_{r=1}^{dn} \sum_{q = 1}^{\floor{dn/r}} if d | qr then 1 else 0
--            = \sum_{r=1}^{dn} floor{ sn / r } where s = gcd(r, d)

-- Identity: \sum_{q = 1}^{\floor{dn/r}} if d | qr then 1 else 0 = ??
-- Proof: LHS = \sum_{q = 1}^{\floor{dn/r}} if (d/s) | q(r/s) then 1 else 0 where s = gcd(r, d)
--            = floor{ floor{dn/r} / (d/s) } where s = gcd(r, d)
--            = floor{ gcd(r, d) n / r }

-- Indentity: \sum_{k=1}^n \sigma_0(kd) = \sum_{r=1}^{dn} \floor{dn/r}
-- Proof: LHS = \sum_{k=1}^n \sum_{q | kd} 1
--            = \sum_{k=1}^n \sum_{1 <= q, r <= kd : qr == kd} 1
--            = 2\sum_{1 <= r <= isqrt(kd)}\sum_{q <= r <= kd : qr == kd} 1

import Control.Monad (when)
import Data.Array qualified as A
import Data.Array.IArray (Array, IArray, Ix, inRange, range)
import Data.Array.IArray qualified as IA
import Data.Array.MArray qualified as MA
import Data.Array.ST (MArray, runSTArray)
import Data.Foldable (Foldable (foldl'), forM_)
import Data.Functor.Identity (runIdentity)
import Data.IntMap.Strict qualified as M
import Data.List (group)
import Data.Stream (Stream (Cons), (<:>))
import Data.Stream qualified as Stream

import Criterion.Main qualified as C

import PrimesInt (combineFactorsGroup, countDivisors', divisorsFromFactorsGroup, groupFactors, primes)

(//) :: Integral a => a -> a -> a
(//) = quot

isq :: Integral a => a -> a
isq = integerSquareRoot

-- -- f == g == h == i == j ??
-- f d r n = sum [numDivisorsProductListMap [k, d] | k <- [1 .. n]]
-- g d r n = sum [(gcd q d * n) // q | q <- [1 .. d * n]]
-- h d r n = 2 * sum [ sum [if (q*r) `mod` d == 0 then 1 else 0 | q <- [r..(d*n) // r]] | r <- [1..isq (d*n)]]
--   - sum [1 | k <- [1..n], k*d == isq (k*d)^2]
-- h d r n =
--   2 * sum [sum [if (q * r) `mod` d == 0 then 1 else 0 | q <- [r .. (d * n) // r]] | r <- [1 .. isq (d * n)]]
--     - isq (n // squareFreePart d)
-- i d r n = 2 * sum [ ((d*n) // r) // (d//s) - sum [if (q * (r//s)) `mod` (d//s) == 0 then 1 else 0 | q <- [1 .. r-1]] | r <- [1..isq (d*n)], let s = gcd r d]
--   - isq (n // squareFreePart d)
-- i d r n =
--   2 * sum [((d * n) // r) // (d // s) - (r - 1) // (d // s) | r <- [1 .. isq (d * n)], let s = gcd r d]
--     - isq (n // squareFreePart d)
-- j d r n =
--   2 * sum [((s * n) // r) - (r - 1) // (d // s) | r <- [1 .. isq (d * n)], let s = gcd r d]
--     - isq (n // squareFreePart d)

-- h d r n = 2 * sum [ sum [ sum [1 | r <- [q..k*d], q*r == k*d] | q <- [1..integerSquareRoot (k*d)]] | k <- [1..n]]
--   - sum [1 | k <- [1..n], k*d == integerSquareRoot (k*d)^2]
-- h d r n = 2 * sum [ sum [ sum [1 | r <- [q..(k*d) // q], q*r == k*d] | q <- [1..isq (k*d)]] | k <- [1..n]]
--   - sum [1 | k <- [1..n], k*d == isq (k*d)^2]
-- i d r n = 2 * sum [ sum [ ((k*d) // s) // (q // s) | q <- [1..isq (k*d)], let s = gcd q d] | k <- [1..n]]
--   - sum [1 | k <- [1..n], k*d == isq (k*d)^2]

-- -- f == g == h
-- f d r n = sum [if (q * r) `mod` d == 0 then 1 else 0 | q <- [1 .. ((d * n) `quot` r)]]
-- g d r n = ((d*n) `quot` r) `quot` (d `quot` gcd d r)
-- h d r n = (gcd d r * n) `quot` r

-- -- f == g
-- f x = sum [numDivisorsProductListMap [n] | n <- [1..x]]
-- g x = 2 * sum [ (x `quot` d) - d | d <- takeWhile (\d -> d^2 < x) [1..]] + integerSquareRoot x


-- Stolen from https://wiki.haskell.org/Generic_number_type#squareRoot
(^!) :: Num a => a -> Int -> a
(^!) x n = x ^ n

integerSquareRoot :: Integral a => a -> a
integerSquareRoot 0 = 0
integerSquareRoot 1 = 1
integerSquareRoot n =
  let twopows = iterate (^! 2) 2
      (lowerRoot, lowerN) =
        last $ takeWhile ((n >=) . snd) $ zip (1 : twopows) twopows
      newtonStep x = div (x + div n x) 2
      iters = iterate newtonStep (integerSquareRoot (div n lowerN) * lowerRoot)
      isRoot r = r ^! 2 <= n && n < (r + 1) ^! 2
   in head $ dropWhile (not . isRoot) iters

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

mutableLookup :: (MArray a b m, Ix i, Eq b) => ((t -> i -> m b) -> t -> i -> m b) -> b -> a i b -> t -> i -> m b
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

memoizeRangeST :: (Ix i, Eq e) => (i, i) -> (forall f. Applicative f => (t -> i -> f e) -> t -> i -> f e) -> e -> t -> i -> e
memoizeRangeST memBounds applicativeF nil qs j = arr IA.! k
 where
  k = fst memBounds
  arr = runSTArray $ do
    stArr <- MA.newArray memBounds nil
    x <- mutableLookup applicativeF nil stArr qs j
    MA.writeArray stArr k x
    return stArr

primeFactorsMemoSTGeneric :: Int -> Int -> [Int]
primeFactorsMemoSTGeneric maxMemo = memoizeRangeST (1, maxMemo) primeFactorsAbstract [-1] primes

primeFactorsMemo :: Int -> [Int]
primeFactorsMemo = primeFactorsMemoSTGeneric maxMemo

primeFactorsGroupMap :: Int -> M.IntMap Int
primeFactorsGroupMap = M.fromAscList . groupFactors . primeFactorsMemo

combineFactorsGroupMap :: (Integral a, Integral b) => M.IntMap a -> M.IntMap a -> M.IntMap b
combineFactorsGroupMap m n = M.map fromIntegral $ M.unionWith (+) m n

divisorsFromFactorsGroupMap :: M.IntMap Int -> [Int]
divisorsFromFactorsGroupMap = M.foldrWithKey (\p e ds -> concatMap (\d -> [d * p ^ k | k <- [0 .. e]]) ds) [1]

numDivisorsMap :: Int -> Int
numDivisorsMap = M.foldr (\e a -> a * (e + 1)) 1 . primeFactorsGroupMap

numDivisorsProductListMap :: [Int] -> Integer
numDivisorsProductListMap ns = M.foldr (\e a -> a * (e + 1)) (1 :: Integer) $ foldr (combineFactorsGroupMap . (M.map fromIntegral . primeFactorsGroupMap)) M.empty ns

divisors :: Int -> [Int]
divisors n = divisorsFromFactorsGroupMap $ primeFactorsGroupMap n

divisorsProductListMap :: [Int] -> [Int]
divisorsProductListMap ns = divisorsFromFactorsGroupMap $ foldr (combineFactorsGroupMap . primeFactorsGroupMap) M.empty ns

squareFreePart :: Int -> Int
squareFreePart n = M.foldrWithKey (\p e a -> a * (p ^ e)) 1 $ M.map (`mod` 2) $ primeFactorsGroupMap n

dfunc1 :: [Int] -> Int -> Int
dfunc1 ms n = sum [sum [(n * gcd d r) `quot` r | r <- [1 .. d * n]] | d <- divisorsProductListMap ms]

dfunc2 :: [Int] -> Int -> Int
dfunc2 ms n = sum [2 * sum [((s * n) // r) - (r - 1) // (d // s) | r <- [1 .. isq (d * n)], let { s = gcd r d }] - isq (n // squareFreePart d) | d <- divisorsProductListMap ms]

-- -- Old memoization with explicit use of the array
-- buildPrimeFactorsArray :: Int -> Array Int [Int]
-- buildPrimeFactorsArray n = runSTArray $ do
--   stArr <- MA.newArray memBounds nil
--   MA.writeArray stArr 1 []
--   forM_ (range memBounds) $ \j -> do
--     ps <- mutableLookup primeFactorsAbstract nil stArr primes j
--     MA.writeArray stArr j ps
--   return stArr
--  where
--   memBounds = (1, n)
--   nil = [-1]

-- buildPrimeFactorsGroupArray :: Int -> Array Int [(Int, Int)]
-- buildPrimeFactorsGroupArray n = groupFactors <$> buildPrimeFactorsArray n

-- TODO: reader monad
-- primeFactors :: Array Int [Int] -> Int -> [Int]
-- primeFactors a n = if inRange (IA.bounds a) n then a IA.! n else undefined -- primeFactorsFunc n

-- primeFactorsGroup :: Array Int [(Int, Int)] -> Int -> [(Int, Int)]
-- primeFactorsGroup a n = if inRange (IA.bounds a) n then a IA.! n else undefined -- groupFactors $ primeFactorsFunc n

-- numDivisors :: Array Int [(Int, Int)] -> Int -> Int
-- numDivisors a n = countDivisors' $ primeFactorsGroup a n

-- numDivisorsProduct :: Array Int [(Int, Int)] -> Int -> Int -> Int
-- numDivisorsProduct a m n = countDivisors' $ combineFactorsGroup (primeFactorsGroup a m) (primeFactorsGroup a n)

-- divisors :: Array Int [(Int, Int)] -> Int -> [Int]
-- divisors a n = product <$> mapM (\(p, e) -> [p ^ k | k <- [0 .. e]]) (primeFactorsGroup a n)

-- divisorsProduct :: Array Int [(Int, Int)] -> Int -> Int -> [Int]
-- divisorsProduct a m n = divisorsFromFactorsGroup $ combineFactorsGroup (primeFactorsGroup a m) (primeFactorsGroup a n)

-- divisorsProductList :: Array Int [(Int, Int)] -> [Int] -> [Int]
-- -- divisorsProductList a ns = divisorsFromFactorsGroup $ foldl' combineFactorsGroup [] $ map (primeFactorsGroup a) ns
-- divisorsProductList a ns = divisorsFromFactorsGroup $ foldr (flip combineFactorsGroup . primeFactorsGroup a) [] ns

-- 2. Implement the D function

-- -- Optimize the number of divisors of a product
-- dfunc2 :: Int -> Int -> Int
-- dfunc2 m n = sum [sum [numDivisorsProduct k d | k <- [1 .. n]] | d <- divisors m]

-- -- Optimize the divisors of a product
-- dfunc3 :: [Int] -> Int -> Int
-- dfunc3 ms n = sum [sum [numDivisorsProduct k d | k <- [1 .. n]] | d <- divisorsProductList ms]

-- dfunc4 :: Array Int [(Int, Int)] -> [Int] -> Int -> Int
-- dfunc4 a ms 1 = sum [numDivisors a d | d <- divisorsProductList a ms]
-- dfunc4 a ms n = dfunc4 a ms (n - 1) + sum [numDivisorsProduct a n d | d <- divisorsProductList a ms]

-- dfunc5 :: Array Int [(Int, Int)] -> Int -> Int -> Int
-- dfunc5 a m n = sum [sum [sum [if (q * r) `mod` d == 0 then 1 else 0 | q <- [1 .. (d * n) `quot` r]] | r <- [1 .. d * n]] | d <- divisors a m]

-- dfunc6 :: Array Int [(Int, Int)] -> Int -> Int -> Int
-- dfunc6 a m n = sum [sum [((d * n) `quot` r) `quot` (d `quot` gcd d r) | r <- [1 .. d * n]] | d <- divisors a m]

-- dfunc7 :: Array Int [(Int, Int)] -> [Int] -> Int -> Int
-- dfunc7 a ms n = sum [sum [sum [if (q * r) `mod` d == 0 then 1 else 0 | q <- [1 .. (d * n) `quot` r]] | r <- [1 .. d * n]] | d <- divisorsProductList a ms]

-- dfunc8 :: Array Int [(Int, Int)] -> [Int] -> Int -> Int
-- dfunc8 a ms n = sum [sum [(n * gcd d r) `quot` r | r <- [1 .. d * n]] | d <- divisorsProductList a ms]

-- Naive implementation is slow.
dfuncNaive :: Int -> Int -> Int
dfuncNaive m n = sum [sum [numDivisorsMap (k * d) | k <- [1 .. n]] | d <- divisors m]

-- Takes about 0.02s
maxMemo :: Int
maxMemo = 10^5

main :: IO ()
main = do
  print $ dfunc2 [2 .. 6] (10 ^ 6)

bmain :: IO ()
bmain =
  C.defaultMain
    [ C.bgroup
        "solve"
        [ C.bench "dfunc1" $ C.nf solve1 [2 .. 6]
        , C.bench "dfunc2" $ C.nf solve2 [2 .. 6]
        ]
    ]
 where
  n = 10 ^ 4
  solve1 ms = dfunc1 ms n
  solve2 ms = dfunc2 ms n
