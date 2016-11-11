import Data.List

list = [2..100]

power :: Integer -> Integer -> Integer
power n 1 = n
power n c = n * power n (c - 1)

process :: [Integer] -> [Integer] -> [Integer]
process (x:xs) []     = process xs list
process []     (y:ys) = []
process (x:xs) (y:ys) = (power x y) : process (x:xs) ys

main = putStr $ show $ length $ nub $ sort $ process list list
-- main = putStr $ show $ length $ process list list
