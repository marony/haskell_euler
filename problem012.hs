import Data.List (sort)

triangle sum s n | s > n     = sum
                 | otherwise = triangle (sum + s) (s + 1) n

factors n = sort (1 : n : filter ((== 0) . (n `mod`)) [2..n `div` 2])

main = putStr $ show $ head $ dropWhile ((< 200) . length) $ map factors $ map (triangle 0 1) [1..]
