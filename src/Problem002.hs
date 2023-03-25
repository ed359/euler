module Problem002 (main) where

fibs :: [Int]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

solve :: Int -> Int
solve n = sum $ filter even $ takeWhile (< n) fibs

main :: IO ()
main = print $ solve 4000000