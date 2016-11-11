import Data.List

is_prime :: Integral a => a -> Bool
is_prime n | n < 2     = False
           | otherwise = is_prime' n 2
               where is_prime' :: Integral a => a -> a -> Bool
                     is_prime' n d
                         | d * d > n      = True
                         | n `mod` d == 0 = False
                         | otherwise      = is_prime' n $ d + 1

primes :: Integral a => [a]
primes = 2 : primes' [3] [3,5..]
    where primes' :: Integral a => [a] -> [a] -> [a]
          primes' (x:xs) ys = ps ++ primes' (xs ++ ps) [z | z <- qs, (z `mod` x) /= 0]
              where (ps, qs) = span (< (x ^ 2)) ys

max_number = 1000000

process :: [Int] -> [[Int]]
process xs = list
    where maximum' [] = []
          maximum' xs = maximum xs
          f n = maximum' $ process' (dropWhile (< n) xs) [] []
          list = map f xs

process' :: [Int] -> [Int] -> [[Int]] -> [[Int]]
process' [] ys zs     = zs
process' (x:xs) [] zs = process' xs [x] zs
process' (x:xs) ys zs = if sum_now > max_number
                        then zs
                        else if is_prime sum_now
                             then process' xs (x:ys) ((x:ys):zs)
                             else process' xs (x:ys) zs
    where sum_now = x + (sum ys)

main :: IO ()
main = putStr $ show $ head $ reverse $ sort $ map (\x -> (length x, sum x, x)) prime
    where primes' = takeWhile (< max_number) primes
          prime = filter ((0 <) . length) $ process primes'
