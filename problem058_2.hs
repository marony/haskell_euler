import Data.Ratio

import Debug.Trace

is_prime n | n < 2     = False
           | otherwise = is_prime' n 2
               where is_prime' :: Integral a => a -> a -> Bool
                     is_prime' n d
                         | d * d > n      = True
                         | n `mod` d == 0 = False
                         | otherwise      = is_prime' n $ d + 1

process minRatio n index maxNumber primeCount | minRatio > ratio = side
                                              | otherwise        = process minRatio (n + 1) (index + 4) ((last numbers) + 1) primeCount'
    where count = (n - 1) * 4 + 1
          side = n * 2 - 1
          numbers = [maxNumber + ((index + 1) `div` 2),
                     maxNumber + ((index + 1) `div` 2) + ((index + 2) `div` 2),
                     maxNumber + ((index + 1) `div` 2) + ((index + 2) `div` 2) + ((index + 3) `div` 2),
                     maxNumber + ((index + 1) `div` 2) + ((index + 2) `div` 2) + ((index + 3) `div` 2) + ((index + 4) `div` 2) - 1]
          primeCount' = primeCount + (length $ filter is_prime numbers)
          ratio = primeCount' % count

main :: IO ()
main = putStrLn $ show $ process 0.10 2 2 2 0
