import Data.Ratio

import Debug.Trace

is_prime :: Integral a => a -> Bool
is_prime n | n < 2     = False
           | otherwise = is_prime' n 2
               where is_prime' :: Integral a => a -> a -> Bool
                     is_prime' n d
                         | d * d > n      = True
                         | n `mod` d == 0 = False
                         | otherwise      = is_prime' n $ d + 1

primes :: [Int]
primes = 2 : primes' [3] [3,5..]
    where primes' :: [Int] -> [Int] -> [Int]
          primes' (x:xs) ys = ps ++ primes' (xs ++ ps) [z | z <- qs, (z `mod` x) /= 0]
              where (ps, qs) = span (< (x ^ 2)) ys

concatNumber :: Integral a => a -> a -> a
concatNumber a b = concatNumber' a b b
    where concatNumber' a b c | b < 10    = a * 10 + c
                              | otherwise = concatNumber' (a * 10) (b `div` 10) c

combination :: [a] -> [[a]]
combination [] = []
combination (x:xs) = [[x,y]| y <- xs] ++ combination xs

--maxPrimeCount = 1000
maxPrimeCount = 150

process :: [[Int]] -> [Int]
process []       = []
process (xs:xss) = if result then xs else process xss
    where combi = combination xs
          result = all (\[x, y] -> (is_prime $ concatNumber x y) && (is_prime $ concatNumber y x)) combi

main :: IO ()
main = putStrLn $ show $ process list
{-
    where list = [[r1, r2, r3, r4, r5] |
                  r1 <- take maxPrimeCount primes,
                  r2 <- take maxPrimeCount $ dropWhile (<= r1) primes,
                  r3 <- take maxPrimeCount $ dropWhile (<= r2) primes,
                  r4 <- take maxPrimeCount $ dropWhile (<= r3) primes,
                  r5 <- take maxPrimeCount $ dropWhile (<= r4) primes,
                  r1 < r2, r2 < r3, r3 < r4, r4 < r5,
                  is_prime $ concatNumber r4 r5,
                  is_prime $ concatNumber r3 r5,
                  is_prime $ concatNumber r2 r5,
                  is_prime $ concatNumber r1 r5,
                  is_prime $ concatNumber r5 r4,
                  is_prime $ concatNumber r5 r3,
                  is_prime $ concatNumber r5 r2,
                  is_prime $ concatNumber r5 r1]
-}
{-
    where list = [[r1, r2, r3, r4] |
                  r1 <- take maxPrimeCount primes,
                  r2 <- take maxPrimeCount $ dropWhile (<= r1) primes,
                  r3 <- take maxPrimeCount $ dropWhile (<= r2) primes,
                  r4 <- take maxPrimeCount $ dropWhile (<= r3) primes,
                  r1 < r2, r2 < r3, r3 < r4,
                  is_prime $ concatNumber r3 r4,
                  is_prime $ concatNumber r2 r4,
                  is_prime $ concatNumber r1 r4,
                  is_prime $ concatNumber r4 r3,
                  is_prime $ concatNumber r4 r2,
                  is_prime $ concatNumber r4 r1]
-}
    where list = [[r1, r2, r3] |
                  r1 <- take maxPrimeCount primes,
                  r2 <- take maxPrimeCount $ dropWhile (<= r1) primes,
                  r3 <- take maxPrimeCount $ dropWhile (<= r2) primes,
                  r1 < r2, r2 < r3,
                  is_prime $ concatNumber r2 r3,
                  is_prime $ concatNumber r1 r3,
                  is_prime $ concatNumber r3 r2,
                  is_prime $ concatNumber r3 r1]

{-
time runhaskell problem060.hs
[3,7,109,673]
runhaskell problem060.hs  124.13s user 0.63s system 99% cpu 2:05.19 total
-}
