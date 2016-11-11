divisors n = filter ((== 0) . (n `mod`)) [1..(n `div` 2)]

d n = sum $ divisors n

main = do putStrLn $ show $ amicable_numbers
          putStrLn $ show $ sums
          putStrLn $ show $ fst sums + snd sums
  where list = [1..10000]
        amicable_numbers = filter (\(x, y) -> (d y) == x && x < y) $ zip list $ map d list
        my_sum = (\(a, b) (c, d) -> (a + c, b + d))
        sums = foldl my_sum (0, 0) amicable_numbers
