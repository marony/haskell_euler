import Data.List

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

is_palindrome :: (Integral a, Show a) => a -> Bool
is_palindrome n = if (length s) /= (length $ nub $ s) then False else is_palindrome' 1 t
    where s = show n
          t = sort s
          is_palindrome' :: (Integral a, Show a) => a -> String -> Bool
          is_palindrome' n []  = True
          is_palindrome' n (x:xs) = if (show n) == [x] then is_palindrome' (n + 1) xs else False

main :: IO ()
main = putStr $ show $ head $ filter (\n -> (is_palindrome n) && (is_prime n)) [987654321,987654319..1]
