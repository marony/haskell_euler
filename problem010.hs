-- prime (x:xs) (y) = prime (filter (\z -> (z `mod` x /= 0)) xs) (y ++ [x])
-- prime [] y       = y

is_prime n = or [(length mods) == 0, all (/= 0) mods]
    where mods = map (n `mod`) [2..truncate $ sqrt $ fromIntegral n]

main = putStr $ show $ foldl (+) 0 $ takeWhile (< 2000000) $ filter is_prime [2..]
