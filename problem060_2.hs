is_prime :: Integral a => a -> Bool
is_prime n | n < 2     = False
           | otherwise = is_prime' n 2
               where is_prime' :: Integral a => a -> a -> Bool
                     is_prime' n d
                         | d * d > n      = True
                         | n `mod` d == 0 = False
                         | otherwise      = is_prime' n $ d + 1

primes :: [Int]
primes = 2 : primes' [3] [3,5..]
    where primes' :: [Int] -> [Int] -> [Int]
          primes' (x:xs) ys = ps ++ primes' (xs ++ ps) [z | z <- qs, (z `mod` x) /= 0]
              where (ps, qs) = span (< (x ^ 2)) ys

concatNumber :: Integral a => a -> a -> a
concatNumber a b = concatNumber' a b b
    where concatNumber' a b c | b < 10    = a * 10 + c
                              | otherwise = concatNumber' (a * 10) (b `div` 10) c

count = 4

process :: [Int] -> [[Int]] -> [[Int]]
process [] as = as
process (p:ps) as = if length ans2 > 0
                    then ans2
                    else process ps ans
    where -- 素数リストがcount個のものを返す
          ans = process' p ([p]:as) []
          ans2 = filter (\xs -> length xs >= count) ans
          -- 素数リストに素数を'加えて'も素数になるリストを作成
          process' :: Int -> [[Int]] -> [[Int]] -> [[Int]]
          process' _ [] ans = ans
          process' p (as:ass) ans = process' p ass $ process'' p as : ans
          -- 素数リストに素数を'加えて'、全パターン素数なら素数をリストに追加
          process'' :: Int -> [Int] -> [Int]
          process'' p xs = if result then (p:xs) else xs
              where combi = concat [[concatNumber x p, concatNumber p x] | x <- xs]
                    result = all is_prime combi

main :: IO ()
main = putStrLn $ show $ (sum ans, ans)
    where ans = concat $ process primes []

{-
% time ./problem060_2
(107623,[47237,41621,7877,5507,5381])
./problem060_2  13883.19s user 182.60s system 74% cpu 5:15:27.92 total
-}
