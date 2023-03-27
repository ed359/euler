module Problem608 (main) where

-- Let D(m,n)=\sum_{d|m}\sum_{k=1}^n\sigma_{0}(kd) where d runs through all divisors of m and \sigma_{0}(n)
-- is the number of divisors of $n$.<br />

-- You are given $D(3!,10^2)=3398$ and $D(4!,10^6)=268882292$.</p>

-- Find D(200!,10^{12}) mod (10^9 + 7)

import Primes (divisors, numDivisors)

-- Naive implementation is slow.
dfunc :: Integral a => a -> a -> Int
dfunc m n = sum [sum [numDivisors (k * d) | k <- [1 .. n]] | d <- divisors m]

main :: IO ()
main = print [dfunc 6 100, dfunc 24 1000000]