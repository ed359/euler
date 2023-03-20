module Problem001 (main, bmain) where

-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.

import Criterion.Main (bench, bgroup, defaultMain, whnf)

-- Galaxy brain solution
sumArith :: Int -> Int -> Int -> Int
sumArith a d n = n * (2 * a + (n - 1) * d) `div` 2

solveGalaxy :: Int -> Int
solveGalaxy n = go 3 + go 5 - go 15
  where
    go d = sumArith d d (nTerms d)
    nTerms = div (n - 1)

-- Wheel solution that avoinds most modding
wheelLength :: Int
wheelLength = 3 * 5

wheel :: [Int]
wheel = [x | x <- [1 .. wheelLength], x `mod` 3 == 0 || x `mod` 5 == 0]

multiples :: [Int]
multiples = concatMap (\i -> [i * wheelLength + x | x <- wheel]) [0 ..]

solveWheel :: Int -> Int
solveWheel n = sum $ takeWhile (< n) multiples

-- Slower solution that does too much modding
solveBasic :: Int -> Int
solveBasic n = sum $ [x | x <- [1 .. (n - 1)], x `mod` 3 == 0 || x `mod` 5 == 0]

main :: IO ()
main = print $ solveGalaxy 1000

bn :: Int
bn = 1000000

bmain :: IO ()
bmain =
  defaultMain
    [ bgroup
        "solve"
        [ bench "basic" $ whnf solveBasic bn,
          bench "wheel" $ whnf solveWheel bn,
          bench "galaxy" $ whnf solveGalaxy bn
        ]
    ]