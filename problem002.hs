fib 1 = 1
fib 2 = 2
fib n = fib (n - 2) + fib (n - 1)

main = putStr $ show $ foldl (+) 0 $ filter even $ takeWhile (< 4000000) $ map fib [1..]
