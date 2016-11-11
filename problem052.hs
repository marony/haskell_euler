import Data.List

is_same_digits :: (Integral a, Show a) => a -> a -> Bool
is_same_digits l r = (sort $ show l) == (sort $ show r)

max_multiply = 6

multiply :: Integer -> Integer -> [Integer] -> [Integer]
multiply n m xs | m > max_multiply        = xs
                | is_same_digits n (n * m) = multiply n (m + 1) (n:xs)
                | otherwise               = []

main :: IO ()
main = putStr $ show $ take 1 $ filter (\n -> length (multiply n 2 []) > 0) [1..]
