import Data.Ratio

import Debug.Trace

is_prime n | n < 2     = False
           | otherwise = is_prime' n 2
               where is_prime' :: Integral a => a -> a -> Bool
                     is_prime' n d
                         | d * d > n      = True
                         | n `mod` d == 0 = False
                         | otherwise      = is_prime' n $ d + 1

-- 1, 2, 3, 5, 7, 10, 13, 17, 21, 26, 31, 37, 43, 50
-- f n = foldl (\l r -> l + r `div` 2) 1 [1..n]
-- coners = [foldl (\l r -> l + r `div` 2) 1 [1..p] | p <- [1..]]
coners = [(if n == 1
          then 1
          else (coners !! (n - 2) + n `div` 2)) | n <- [1..]]

is_primes = [is_prime $ if ((n - 2) `mod` 4) == 0 then (coners !! (n - 1)) - 1 else coners !! (n - 1) | n <- [1..]]

-- prime_counts = [length $ filter (== True) $ take n is_primes | n <- [1..]]
prime_counts = [(if n == 1
                 then 0
                 else
                    (prime_counts !! (n - 2) + (if is_primes !! (n - 1) 
                                             then 1 
                                             else 0))) | n <- [1..]]

percent 1 = 0.0
percent n = if (n `mod` 1000) == 0 then trace ((show n) ++ " = " ++ (show (((fromIntegral (g' count)) :: Double) / ((fromIntegral count) :: Double)))) ((fromIntegral (g' count)) :: Double) / ((fromIntegral count) :: Double)
                                   else ((fromIntegral (g' count)) :: Double) / ((fromIntegral count) :: Double)
-- percent n = (g' n) % count
    where count = (n - 1) * 4 + 1
--          h' x = is_prime $ if ((x - 2) `mod` 4) == 0 then (coners !! (x - 1)) - 1 else coners !! (x - 1)
          g' 1 = 0
--          g' n = length $ filter h' [1..count]
          g' n = prime_counts !! (n - 1)

main :: IO ()
main = putStrLn $ show $ take 1 $ dropWhile (\x -> snd x >= 0.13) $ drop 1 $ zip [1,3..] $ map percent [1..]
-- main = putStrLn $ show $ take 55 $ drop 1 $ zip [1,3..] $ map percent [1..]
-- main = putStrLn $ show $ take 20 $ zip is_primes prime_counts
