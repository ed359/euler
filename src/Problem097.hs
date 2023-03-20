module Problem097 (main) where

-- The first known prime found to exceed one million digits was discovered in
-- 1999, and is a Mersenne prime of the form 26972593−1; it contains exactly
-- 2,098,960 digits. Subsequently other Mersenne primes, of the form 2^p−1,
-- have been found which contain more digits.

-- However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits: 28433×2^7830457+1.

-- Find the last ten digits of this prime number.

-- This is too easy for GHC
-- Strangely, compiling and running I get a segfault on GHC including 9.2.5 and 9.4.4. But in GHCI it gives 8739992577 quickly
p :: Integer
p = 28433 * 2^7830457 + 1

main :: IO ()
main = print $ p `mod` 10^10