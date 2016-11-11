import Data.List

fact :: Integer -> Integer
fact 0 = 1
fact n = fact' n n
    where fact' n 1 = n
          fact' n a = fact' (n * (a - 1)) (a - 1)

ncr :: Integer -> Integer -> Integer
ncr n r = fact n `div` (fact r * fact (n - r))

main :: IO ()
main = putStr $ show $ length $ filter (>1000000) $ concat $ map (\x -> map (\y -> (ncr x y)) [1..(x - 1)]) [1..100]
