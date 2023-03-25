module Problem077 (main, primePartitions, naivePrimePartitions) where

-- It is possible to write ten as the sum of primes in exactly five different ways:

-- 7 + 3
-- 5 + 5
-- 5 + 3 + 2
-- 3 + 3 + 2 + 2
-- 2 + 2 + 2 + 2 + 2

-- What is the first value which can be written as the sum of primes in over five thousand different ways?

import Primes (primes)

import Data.Map qualified as M
import Data.Set qualified as S

-- see e.g. https://hackage.haskell.org/package/multiset
-- a multiset of Int's would be better off being implemented via IntMap from containers
newtype MultiSet a = MultiSet (M.Map a Int)
    deriving (Eq, Ord)

instance Show a => Show (MultiSet a) where
    showsPrec p xs = showParen (p > 10) $ showString "fromList " . shows (toList xs)

instance Foldable MultiSet where
    foldr f z (MultiSet m) = M.foldrWithKey repF z m
      where
        repF x 1 acc = f x acc
        repF x n acc = repF x (n - 1) (f x acc)
    foldl f z (MultiSet m) = M.foldlWithKey repF z m
      where
        repF acc x 1 = f acc x
        repF acc x n = repF (f acc x) x (n - 1)

empty :: MultiSet a
empty = MultiSet M.empty

insert :: Ord a => a -> MultiSet a -> MultiSet a
insert x (MultiSet m) = MultiSet (M.insertWith (+) x 1 m)

toList :: MultiSet a -> [a]
toList = foldr (:) []

fromList :: Ord a => [a] -> MultiSet a
fromList = foldr insert empty

naivePrimePartitions :: Int -> S.Set (MultiSet Int)
naivePrimePartitions 0 = S.singleton empty
naivePrimePartitions 1 = S.empty
naivePrimePartitions n = S.fromList [p `insert` ps | p <- takeWhile (<= n) primes, ps <- S.toList $ naivePrimePartitions (n - p)]

-- Memoize the above algorithm
primePartitions :: Int -> S.Set (MultiSet Int)
primePartitions n = map f [0 ..] !! n
  where
    f :: Int -> S.Set (MultiSet Int)
    f 0 = S.singleton empty
    f 1 = S.empty
    f n = S.fromList [p `insert` ps | p <- takeWhile (<= n) primes, ps <- S.toList $ primePartitions (n - p)]

-- Amazing code from https://oeis.org/A000607
-- go (p:ps) m gives the number of ways of writing m as a sum of the primes occurring in (p:ps)
-- go _ 0 = 1 for the empty sum, clearly
-- for the recursive case, either we take the prime p from the head of the supplied list of primes,
-- use it and keep using the entire list (p:ps) to make (m-p), or we do not use any copies of p and just make m
-- this avoids any need for deduping and hence immediately simplifies down to counting rather than merging sets
numberOfPrimePartitions :: Integral a => a -> a
numberOfPrimePartitions = go primes
  where
    go _ 0 = 1
    go (p : ps) m = if m < p then 0 else go (p : ps) (m - p) + go ps m
{-# SPECIALIZE numberOfPrimePartitions :: Int -> Int #-}
{-# SPECIALIZE numberOfPrimePartitions :: Integer -> Integer #-}

solve :: Int -> Int
solve n = fst . head $ dropWhile (\(_, pps) -> pps <= n) (map (\m -> (m, numberOfPrimePartitions m)) [2 ..])

main :: IO ()
main = print $ solve 5000