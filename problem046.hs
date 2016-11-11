import Data.List
import Debug.Trace

is_prime :: Integral a => a -> Bool
is_prime n | n < 2     = False
           | otherwise = is_prime' n 2
               where is_prime' :: Integral a => a -> a -> Bool
                     is_prime' n d
                         | d * d >= n     = True
                         | n `mod` d == 0 = False
                         | otherwise      = is_prime' n $ d + 1

primes :: Integral a => a -> [a]
primes n = filter is_prime [2..n]

process :: (Integral a, Show a) => a -> Bool
process n = any (process' n) (primes n)
    where process' :: (Integral a, Show a) => a -> a -> Bool
          process' n p = any (process'' n p) [1..n]
          process'' :: (Integral a, Show a) => a -> a -> a -> Bool
          process'' n p m = if n == p + 2 * (m ^ 2) then True else False

main :: IO ()
main = putStr $ show $ head $ dropWhile (\x -> snd x == True) $ zip list $ map process list
    where list = [x | x <- [3,5..], not $ is_prime x]
