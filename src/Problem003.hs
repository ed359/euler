module Problem003 (main) where

-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?

import Primes (primeFactors)

solve :: Integer
solve = maximum $ primeFactors 600851475143

main :: IO ()
main = print solve