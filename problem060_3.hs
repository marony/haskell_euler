import Debug.Trace

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

-- 順列
permutation :: [a] -> Int -> [[a]]
permutation [] _    = [[]]
permutation ns size = perm size (length ns - 1) [(ns, [])]
    where
      perm 0 _ xs = [a | (_, a) <- xs]
      perm c n xs = perm (c - 1) (n - 1) $ concatMap (f n) xs
      f n (xs, ys) = [(as ++ bs, ys ++ [b]) |
                      x <- [0 .. n], let (as, b : bs) = splitAt x xs]

-- 組み合わせ
combination :: [a] -> Int -> [[a]]
combination [] _ = [[]]
combination ns size = comb size [(ns, [])]
    where
      comb 0 xs = [a | (_, a) <- xs]
      comb c xs = comb (c - 1) $ concatMap comb' xs
      comb' (x : xs, ys) = (xs, ys ++ [x]) : comb' (xs, ys)
      comb' _ = []

count = 4

process :: [Int] -> [[Int]] -> [Int]
process [] _ = []
process (p:ps) as = if length result1 >= count && length result2 > 0
                    then result2
                    else process ps (result1 : as)
--    where result1 = trace ("process' p = " ++ (show p) ++ ", as = " ++ (show as) ++ ", result1 = " ++ (show $ process' p as)) process' p as
    where result1 = process' p as
          comb = combination result1 count
--          comb = trace ("comb1 = " ++ (show $ combination result1 count)) combination result1 count
          result2 = check' comb
          -- 自分と組み合わせたら素数になる素数をリスト化
          process' :: Int -> [[Int]] -> [Int]
--          process' p ass = trace ("process' p = " ++ (show p) ++ ", ass = " ++ (show ass)) p : filter (/= 0) (map f ass)
          process' p ass = p : filter (/= 0) (map f ass)
          f :: [Int] -> Int
          f [] = 0
          f as = if (is_prime $ concatNumber p (head as)) && (is_prime $ concatNumber (head as) p)
                 then head as
                 else 0
          -- 全ての組み合わせが素数か調査
          check' :: [[Int]] -> [Int]
          check' [] = []
          check' (xs:xss) = if all (\[x, y] -> (is_prime $ concatNumber x y) && (is_prime $ concatNumber y x)) comb
                            then xs
                            else check' xss
              where comb = combination xs 2
--              where comb = trace ("comb2 = " ++ (show $ combination xs 2)) combination xs 2

main :: IO ()
main = putStrLn $ show $ (sum ans, ans)
    where ans = process primes []
--    where ans = process (takeWhile (<= 673) primes) []

{-
* problem_60_3.hs
(107,[67,37,3])

* problem_60_2.hs
(123,[97,19,7])
(792,[673,109,7,3])

process' p = 2, as = []
process' p = 2, ass = []
process' p = 3, as = [[2]]
process' p = 3, ass = [[2]]
process' p = 5, as = [[3],[2]]
process' p = 5, ass = [[3],[2]]
process' p = 7, as = [[5],[3],[2]]
process' p = 7, ass = [[5],[3],[2]]
process' p = 11, as = [[7,3],[5],[3],[2]]
process' p = 11, ass = [[7,3],[5],[3],[2]]
process' p = 13, as = [[11,3],[7,3],[5],[3],[2]]
process' p = 13, ass = [[11,3],[7,3],[5],[3],[2]]
process' p = 17, as = [[13],[11,3],[7,3],[5],[3],[2]]
process' p = 17, ass = [[13],[11,3],[7,3],[5],[3],[2]]
process' p = 19, as = [[17,3],[13],[11,3],[7,3],[5],[3],[2]]
process' p = 19, ass = [[17,3],[13],[11,3],[7,3],[5],[3],[2]]
process' p = 23, as = [[19,13,7],[17,3],[13],[11,3],[7,3],[5],[3],[2]]
process' p = 23, ass = [[19,13,7],[17,3],[13],[11,3],[7,3],[5],[3],[2]]
process' p = 29, as = [[23,11],[19,13,7],[17,3],[13],[11,3],[7,3],[5],[3],[2]]

% time ./problem060_2
(107623,[47237,41621,7877,5507,5381])
./problem060_2  13883.19s user 182.60s system 74% cpu 5:15:27.92 total
-}
