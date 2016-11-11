-- prime (x:xs) (y) = prime (filter (\z -> (z `mod` x /= 0)) xs) (y ++ [x])
-- prime [] y       = y

is_prime :: Integral a => a -> Bool
is_prime n = or [(length mods) == 0, all (/= 0) mods]
    where mods = map (n `mod`) [2..truncate $ sqrt $ fromIntegral n]

main = putStr $ show $ head $ reverse $ take 10001 $ filter is_prime [2..]
