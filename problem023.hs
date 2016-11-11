import Debug.Trace

list = [1..28123]

divisors n = filter ((== 0) . (n `mod`)) [1..(n `div` 2)]
-- divisors n = strict_list [x `seq` x | x <- [1..(n `div` 2)], n `mod` x == 0]

is_perfect_number n = n == (sum $ divisors n)
is_deficient_number n = n > (sum $ divisors n)
is_abundant_number n = n < (sum $ divisors n)

abundant_numbers = filter is_abundant_number list

not_sum_of_two_abundant_numbers []     []     zs = zs
not_sum_of_two_abundant_numbers (x:xs) []     zs = not_sum_of_two_abundant_numbers (x:xs) abundant_numbers zs
not_sum_of_two_abundant_numbers []     (y:ys) zs | length ys == 0 = zs
                                                 | otherwise      = trace (show $ length zs) (not_sum_of_two_abundant_numbers abundant_numbers ys zs)
not_sum_of_two_abundant_numbers (x:xs) (y:ys) zs = not_sum_of_two_abundant_numbers xs (y : ys) $ filter (x + y /=) zs

main = do putStrLn $ show $ length $ not_sum_of_two_abundant_numbers abundant_numbers abundant_numbers list
          putStrLn $ show $ sum $ not_sum_of_two_abundant_numbers abundant_numbers abundant_numbers list
