import Data.List
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

count = 5

process :: [Int] -> [[Int]] -> [Int]
process [] _ = []
process (p:ps) as = if length result2 >= count
                    then result2
                    else process ps (result1 : as)
--    where result1 = trace ("process' p = " ++ (show p) ++ ", as = " ++ (show as) ++ ", result1 = " ++ (show $ process' p as)) process' p as
    where result1 = process' p as
          result2 = process'' p result1 as
          -- 自分と組み合わせたら素数になる素数をリスト化
          process' :: Int -> [[Int]] -> [Int]
--          process' p ass = trace ("process' p = " ++ (show p) ++ ", ass = " ++ (show ass)) p : filter (/= 0) (map f ass)
          process' p ass = trace ("process' p = " ++ (show p)) p : filter (/= 0) (map f ass)
--          process' p ass = p : filter (/= 0) (map f ass)
          -- 今の素数とリストの先頭が組み合わせたら素数になるか
          f :: [Int] -> Int
          f [] = 0
          f as = if (is_prime $ concatNumber p (head as)) && (is_prime $ concatNumber (head as) p)
                 then head as
                 else 0
          -- 今の素数リストと共通の要素がある(count - 1)個ある要素リストを探す
          process'' :: Int -> [Int] -> [[Int]] -> [Int]
--          process'' _ (p:ps) ass | length ps < count - 1 = trace ("process'' p = " ++ (show p) ++ ", ps = " ++ (show ps) ++ ", ass = " ++ (show ass)) []
          process'' _ (p:ps) ass | length ps < count - 1 = trace ("process'' p = " ++ (show p) ++ ", ps = " ++ (show ps)) []
--          process'' _ (p:ps) ass | length ps < count - 1 = []
--                                 | otherwise             = trace ("process'' p = " ++ (show p) ++ ", ps = " ++ (show ps) ++ ", ass = " ++ (show ass)) p : result
                                 | otherwise             = trace ("process'' p = " ++ (show p) ++ ", ps = " ++ (show ps)) p : result
--                                 | otherwise             = p : result
              -- 本当はconcatじゃダメ。二つ以上答えが同時に見つかったらダメになる
--              where result = trace ("result = " ++ (show $ filter g ass)) ps `intersect` (concat $ filter g ass)
              where result = ps `intersect` (concat $ filter g ass)
                    g as = g' as (count - 1)
                    g' [] _ = True
                    g' (a:as) n = (length (a : as) >= n) && (a `elem` ps) && (length $ ps `intersect` (a : as)) >= n && g' as (n - 1)

main :: IO ()
main = putStrLn $ show $ (sum ans, ans)
    where ans = process primes []
--    where ans = process (takeWhile (<= 673) primes) []

{-
* problem060_5.hs
% time runhaskell problem060_5.hs
(792,[673,109,7,3])

* problem_60_3.hs
(107,[67,37,3])

* problem_60_2.hs
(123,[97,19,7])
(792,[673,109,7,3])

process'' p = 673, ps = [613,499,457,397,199,109,7,3], ass = [[661,613,547,541,439,421,373,139,109],[659,653,569,521,173,137],[653,503,491,461,311,281,53],[647,617,593,509,503,263,113],[643,547,463,457,421,73],[641,521,467,239,197,167,131,101],[631,307,229,151,79],[619,607,313,181,139,67],[617,587,467,293,269,233,131,3],[613,577,463,439,367,229,163,79,43,3],[607,421,337,181,151,127,73,37,3],[601,487,439,283,241,193,127,67],[599,479,419,359,191,29],[593,491,479,401,227,179,41],[587,563,467,137,11],[577,547,523,331,307,193,19,13],[571,541,433,211,157,73,19],[569,479,461,197,29],[563,503,449,419,359,149,83],[557,521,449,281,3],[547,397,229,223,139,73,67,7],[541,523,439,283,193,7,3],[523,463,307,13,7],[521,167,89,47],[509,359,281,239,23],[503,317,11],[499,349,277,229,211,181,151,43,3],[491,251,167,149,137,17],[487,283,61,7],[479,461,431,131],[467,293,101,3],[463,283,37],[461,269,239,191],[457,367,139],[449,419,131,107,83,17,3],[443,419,353,347,263,167,89,83,71],[439],[433,421,229,193,151,19,7],[431,269,251,173,89,17],[421,241,181,103],[419,383,317,281,59,53,47],[409,349,271,163,61],[401,347,53,29],[397,379,337,283,181,151,79],[389,269,71,17],[383,179,113,101,29,17],[379,199,97],[373,349,229,211,199,193,127,97,3],[367,307,163,139,79,13],[359,353,311,137,101,3],[353,317,137,53,11],[349,337,331,211],[347,251,239,233,197,173,29],[337,223,13],[331,277,127,61,13,3],[317,269,179,71],[313,241,211,109,37],[311,293,197,83,23],[307,163,103],[293,263,257,173,89,47],[283,211,193,181,7],[281,227,191],[277,223,157,73,37],[271,241,211,127,43,3],[269,179,167,53,47],[263,257,239,71],[257,71,41,17],[251,233,191,149,47,11],[241,127,97,79,13],[239,233,137,17,11],[233,113,71],[229,223,157,7,3],[227,191,113,83,41],[223,43],[211,199],[199,181,109,37],[197,137,101,59,53],[193,181,163,79],[191,173,137,3],[181,157,31,19],[179,29],[173,149],[167,113,59,29],[163,151,127,19],[157,127,97,67],[151,61,31],[149,113,101,47],[139,109,67,31],[137,89,29,3],[131,113],[127,13,7],[113,53,11],[109,7,3],[107,101,89],[103,43,13],[101],[97,43,19,7],[89,23],[83,17],[79,37,19],[73,3],[71,29],[67,37,3],[61,13,7],[59,3],[53],[47,23],[43],[41],[37,3],[31,19,3],[29],[23,11],[19,13,7],[17,3],[13],[11,3],[7,3],[5],[3],[2]]

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
