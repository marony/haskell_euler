is_prime n = (length (filter (== 0) $ map (n `mod`) [2..(n - 1)]) == 0)

primes n = filter is_prime [2..(truncate $ sqrt $ fromIntegral n)]
 
prime_factors 1 _      = [] --[1]
prime_factors _ []     = []
prime_factors n (x:xs) = if (n `mod` x) == 0 then
                             x : prime_factors (n `div` x) (x : xs)
                         else
                             prime_factors n xs
 
main = putStr $ show $ maximum $ prime_factors n (primes n)
    where n = 600851475143
