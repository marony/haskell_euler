import Data.List
import Debug.Trace

is_prime :: Integral a => a -> Bool
is_prime n | n < 2     = False
           | otherwise = is_prime' n 2
               where is_prime' :: Integral a => a -> a -> Bool
                     is_prime' n d
                         | d * d > n      = True
                         | n `mod` d == 0 = False
                         | otherwise      = is_prime' n $ d + 1

is_permutation :: (Integral a, Show a) => a -> a -> a -> Bool
is_permutation x y z = sx == sy && sy == sz
    where sx = sort $ show x
          sy = sort $ show y
          sz = sort $ show z

main :: IO ()
main = putStr $ show $ map (\(x, y, z) -> (x, y, z, (show x ) ++ (show y) ++ (show z))) list
    where primes = filter is_prime [1001,1003..9999]
          list = [(x, y, z) | x <- primes,
                              y <- dropWhile (<= x) primes,
                              z <- dropWhile (<= y) primes,
                              z - y == y - x,
                              is_permutation x y z]
