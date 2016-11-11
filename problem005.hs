main = putStr $ show $ (+ 1) . last $ takeWhile (\x -> (length $ filter (/= 0) $ map (mod x) [1..20]) /= 0) [1..]
